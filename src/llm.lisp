;;; src/llm.lisp
;;;
;;; Minimal client for the local llama.cpp server (OpenAI-compatible
;;; /v1/chat/completions endpoint). Designed to be used by any process
;;; in the system that wants to ask a local model a question; doesn't
;;; assume any particular application (no domain knowledge baked in).
;;;
;;; The current host model is Qwen3.6-35B-A3B (35B MoE / 3B active,
;;; Q4_K_M, 262K context). Two relevant quirks:
;;;   - Qwen emits the reasoning trace into MESSAGE.REASONING_CONTENT
;;;     and the final answer into MESSAGE.CONTENT; if MAX_TOKENS is
;;;     too small the reasoning eats the whole budget and CONTENT is
;;;     empty. With THINK NIL we set chat_template_kwargs.enable_thinking
;;;     so the model skips reasoning entirely.
;;;   - json-simple serializes CL T -> JSON true and CL NIL -> JSON false
;;;     (NOT null). Keep that in mind when building bodies.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  ;; #+QUICKLISP: the .asd already loads these; this is for loading the file by hand.  Guarded
  ;; because READING `ql:' is an error in an image without Quicklisp, before anything runs.
  #+quicklisp (ql:quickload '(:json-simple) :silent t))

(defpackage #:operandi.llm
  (:use #:cl)
  (:local-nicknames (#:json #:json-simple) (#:http #:operandi.http))
  (:export #:*llm-url* #:*llm-default-max-tokens* #:*llm-read-timeout*
           #:*llm-auth-token* #:*llm-model* #:*llm-backend*
           #:*llm-effort* #:parse-effort #:apply-reasoning
           #:llm-chat #:ht #:extract-json
           #:image-part #:text-part #:user-content #:content-text #:content-images
           #:image-file-p
           #:set-backend-openrouter! #:set-backend-llama! #:set-backend-routstr!
           #:set-backend-ppq! #:set-backend-tinfoil!
           #:with-llama #:with-openrouter #:with-routstr #:with-ppq
           #:*routstr-url*
           #:openrouter-credits
           #:cloud-backend-p #:*sealed-post* #:post-chat
           #:*confidential* #:confidential-backend-p
           #:enable-confidential! #:require-confidential!
           #:*usage-sink* #:usage #:make-usage #:copy-usage
           #:usage-calls #:usage-prompt-tokens #:usage-completion-tokens
           #:usage-cost-usd #:usage-cached-tokens #:with-usage-accounting
           #:fold-usage #:usage-incf #:usage-summary))

(in-package #:operandi.llm)

(defparameter *llm-url* "http://127.0.0.1:8081/v1/chat/completions")
(defparameter *llm-default-max-tokens* 256)
(defparameter *llm-read-timeout* 120)

;;; ---- message content ------------------------------------------------------
;;; A message's "content" is a string, or — once images are involved — a
;;; vector of OpenAI-style parts: {"type":"text","text":…} and
;;; {"type":"image_url","image_url":{"url":"data:image/png;base64,…"}}.
;;; Everything that reads content goes through CONTENT-TEXT so it never has
;;; to care which.

(defparameter *image-types*
  '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("gif" . "image/gif") ("webp" . "image/webp"))
  "Extensions the vision endpoints accept, and their MIME types.")

(defun image-file-p (path)
  "The MIME type if PATH has an image extension, else NIL."
  (let ((type (pathname-type (pathname path))))
    (and type (cdr (assoc type *image-types* :test #'string-equal)))))

(defun text-part (text) (ht "type" "text" "text" text))

(defun image-part (path)
  "An image_url content part carrying PATH's bytes as a data: URL."
  (let ((mime (or (image-file-p path)
                  (error "not an image file (png/jpg/gif/webp): ~A" path)))
        (bytes (with-open-file (in path :element-type '(unsigned-byte 8))
                 (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                   (read-sequence v in)
                   v))))
    (ht "type" "image_url"
        "image_url" (ht "url" (format nil "data:~A;base64,~A" mime
                                      (cl-base64:usb8-array-to-base64-string bytes))))))

(defun user-content (text &optional image-paths)
  "TEXT alone stays a plain string; with IMAGE-PATHS it becomes a parts
   vector, text first."
  (if (null image-paths)
      text
      (coerce (cons (text-part text) (mapcar #'image-part image-paths)) 'vector)))

(defun content-text (content)
  "The textual side of CONTENT as a string: a string is itself, a parts
   vector is its text parts joined with each image reduced to [image],
   JSON null / NIL is \"\"."
  (cond ((stringp content) content)
        ((or (null content) (eq content 'null)) "")
        ((or (vectorp content) (listp content))
         (with-output-to-string (o)
           (map nil (lambda (part)
                      (cond ((not (hash-table-p part)) (princ part o))
                            ((equal (gethash "type" part) "text")
                             (write-string (or (gethash "text" part) "") o))
                            ((equal (gethash "type" part) "image_url")
                             (write-string " [image] " o))))
                content)))
        (t (princ-to-string content))))

(defun content-images (content)
  "How many image parts CONTENT carries."
  (if (or (vectorp content) (and (listp content) content))
      (count-if (lambda (p) (and (hash-table-p p) (equal (gethash "type" p) "image_url")))
                content)
      0))

(defparameter *llm-backend* :llama
  ":LLAMA = local llama.cpp at *llm-url* (default).
   :OPENROUTER = OpenRouter API; *llm-url*, *llm-auth-token*, and
   *llm-model* must be set. Use USE-OPENROUTER to flip.
   :ROUTSTR = Routstr (api.routstr.com/v1, OpenAI-compatible, Cashu-paid).
   Same request shape as OpenRouter; the bearer token is a Cashu token or
   an sk- API key.
   :PPQ = PPQ (ppq.ai), OpenAI-compatible, paid in Bitcoin/Lightning.  By
   default it is an ordinary hosted provider: *llm-model* is any PPQ model id
   and the request goes out in the clear, exactly like OpenRouter.
   With *CONFIDENTIAL* bound true (the --confidential flag) it becomes Path A
   only: *llm-model* must be a private/* id, every request is EHBP-sealed to
   the HPKE key of an AMD-attested Tinfoil enclave verified locally against
   the pinned AMD ARK, and there is NO fallback — an unverifiable enclave or a
   plaintext answer is an error, not a downgrade.  See OPERANDI.PPQ.")

(defun cloud-backend-p (&optional (backend *llm-backend*))
  "True for hosted backends that speak OpenRouter's request dialect
   (usage accounting, reasoning.effort): :OPENROUTER and :ROUTSTR — and
   :PPQ and :TINFOIL, which are hosted and metered too (they just may seal
   the body)."
  (member backend '(:openrouter :routstr :ppq :tinfoil)))

(defvar *confidential* nil
  "When true, the current backend must be able to prove it is talking to a
   measured enclave, and every request must be sealed to it.  Set by the
   --confidential flag (and by WITH-PPQ :CONFIDENTIAL T).

   This is a property of the SESSION, not of the backend: it is what turns
   :PPQ from \"a hosted provider\" into \"Path A only, no fallback\".  It is
   deliberately separate from the backend choice so that a caller can say
   \"I do not care which provider, but I do care that it is attested\" and
   have that be a checkable claim rather than a naming convention.")

(defun confidential-backend-p (&optional (backend *llm-backend*) (model *llm-model*))
  "True when BACKEND can actually satisfy *CONFIDENTIAL* for MODEL.  :PPQ (Path
   A) and :TINFOIL can: they are the backends whose transport we can seal to a
   locally verified enclave, and in both cases the key we seal to is the key
   bound into the attestation report we verified.

   :ROUTSTR cannot, and MODEL is ignored.  A Routstr node's `tinfoil-*` models
   ARE relayed blind to a genuine Tinfoil enclave — we have driven that round
   trip and the answer comes back sealed — but the key we can seal to is the
   ROUTER's (inference.tinfoil.sh), not the model enclave's.  The router then
   re-seals to the per-model container, and that second hop is a trust hop we
   cannot check from here: the container's own TDX quote is not verifiable
   against a pinned root the way the SEV-SNP one is, and the router's claim
   about which container it picked is exactly the thing in question.  So a
   Routstr request is encrypted, but not end-to-end attested, and --confidential
   must not accept it.  :OPENROUTER and :LLAMA cannot seal at all.  In every
   case --confidential with a backend that cannot prove the claim is a
   configuration error, not a silent downgrade."
  (case backend
    ((:ppq :tinfoil) t)
    (t nil)))

(defun enable-confidential! ()
  "Turn on confidential mode for the current session (the --confidential flag).

   If the backend is already :PPQ or :ROUTSTR, this also (re)installs the sealer
   and re-checks that the current model is one that can be served confidentially
   — so `--ppq X --confidential` and `--confidential --ppq X` behave
   identically.  On any other backend it just sets the flag;
   REQUIRE-CONFIDENTIAL! then fails the run at startup rather than letting
   plaintext out."
  (setf *confidential* t)
  (case *llm-backend*
    (:ppq
     (unless (operandi.ppq:confidential-p *llm-model*)
       (error "confidential mode: ~s is not a private/* model — Path B (PPQ's own \
enclave, plaintext upstream) is not allowed. Use --ppq private/<model>."
              *llm-model*))
     (setf *llm-url* (format nil "~A/v1/chat/completions" operandi.ppq:*ppq-private-base*)
           *sealed-post* (lambda (url body headers)
                           (operandi.ppq:sealed-post
                            url body headers :base operandi.ppq:*ppq-private-base*
                            :model (operandi.ppq:ppq-wire-model *llm-model*)))))
    (:routstr
     (error "confidential mode: a Routstr node is not an attested transport.  Its \
tinfoil-* models are relayed blind to a real Tinfoil enclave, but the key we can \
seal to is the ROUTER's, and the router re-seals to the per-model container — a \
second hop we cannot verify from here.  The request is encrypted, not \
end-to-end attested.  Use --tinfoil <model> or --confidential --ppq \
private/<model>, or drop --confidential to use the node as an ordinary \
provider.")))
  t)

(defun require-confidential! ()
  "Signal unless the current backend can honour *CONFIDENTIAL*.  Called at
   startup so that `--confidential --openrouter ...` fails immediately and
   loudly instead of quietly sending plaintext for the whole session."
  (when (and *confidential* (not (confidential-backend-p)))
    (error "confidential mode is not available on the ~(~a~) backend~@[ for model ~s~]: \
only PPQ (Path A) and Tinfoil seal to a key that is bound into an attestation \
report we verify locally. Use --confidential --ppq private/<model>, \
--tinfoil <model>, or drop --confidential.  (A Routstr node's tinfoil-* models \
are encrypted but not end-to-end attested — the router re-seals to the model \
container — so they do not satisfy --confidential.)"
           *llm-backend* (and (eq *llm-backend* :routstr) *llm-model*)))
  t)

(defparameter *llm-effort* nil
  "Reasoning effort for the AGENT LOOP: NIL (leave it to the provider/model
   default), :OFF, :LOW, :MEDIUM, or :HIGH. Seeded from OPERANDI_EFFORT; the
   CLI's --effort and the TUI's /effort set it. What it means per backend:
     OpenRouter — reasoning.enabled=false for :OFF, reasoning.effort for the
       rest (ignored by models without reasoning; models that REQUIRE it,
       e.g. Minimax M2.x, 400 on :OFF).
     llama.cpp — chat_template_kwargs.enable_thinking (Qwen): on for
       :LOW/:MEDIUM/:HIGH, off for :OFF — and off for NIL too, which is the
       long-standing default there: a small local model's trace eats the
       output budget before the first tool_call byte.")

(defun parse-effort (s)
  "\"off\"|\"low\"|\"medium\"|\"high\" -> keyword; \"default\" -> NIL.
   Second value NIL if S wasn't one of those."
  (let ((k (and (stringp s) (string-downcase (string-trim " " s)))))
    (cond ((member k '("off" "none" "no" "0") :test #'string=) (values :off t))
          ((string= k "low")     (values :low t))
          ((string= k "medium")  (values :medium t))
          ((string= k "high")    (values :high t))
          ((member k '("default" "auto") :test #'string=) (values nil t))
          (t (values nil nil)))))

(setf *llm-effort* (let ((e (uiop:getenv "OPERANDI_EFFORT"))) (and e (parse-effort e))))

(defun apply-reasoning (body &optional (effort *llm-effort*))
  "Set the backend-specific reasoning fields on request BODY (a hash table)
   for EFFORT, and return BODY. The single place the two dialects live."
  (case *llm-backend*
    ((:openrouter :routstr)
     (case effort
       (:off (setf (gethash "reasoning" body) (ht "enabled" nil)))
       ((:low :medium :high)
        (setf (gethash "reasoning" body) (ht "effort" (string-downcase (symbol-name effort)))))))
    (:llama
     (setf (gethash "chat_template_kwargs" body)
           (ht "enable_thinking" (and (member effort '(:low :medium :high)) t))))
    ;; :PPQ and :TINFOIL speak the enclave's plain OpenAI dialect: no
    ;; reasoning field, no chat_template_kwargs.  An unknown field is a 400 we
    ;; would have to debug through a sealed channel, so send nothing.
    ((:ppq :tinfoil) body))
  body)

(defparameter *llm-auth-token* nil
  "Bearer token for the LLM endpoint, or NIL. Required for openrouter.")

(defparameter *llm-model* nil
  "Model name string. Required for openrouter. Cheapest-first picks:
   minimax/minimax-m2.7 ($0.30/M in), deepseek/deepseek-v4-pro ($0.45),
   moonshotai/kimi-k2.6 ($0.75), anthropic/claude-sonnet-4-5 ($3 — 10x).
   NIL on local llama.cpp.")

(defun ht (&rest pairs)
  "Build a string-keyed hash-table for json-simple serialization."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k h) v))
    h))

;;; ----------------------- usage accounting ----------------------------
;;; When *USAGE-SINK* is bound to a USAGE struct, every LLM-CHAT call adds
;;; its token counts and (OpenRouter-reported) USD cost into it. Lets a
;;; caller meter the LLM cost of a whole action without touching the call
;;; sites. Non-invasive: nil sink = no-op.

(defstruct usage
  (calls 0) (prompt-tokens 0) (completion-tokens 0) (cost-usd 0d0)
  (cached-tokens 0))   ; prompt tokens served from a provider prefix cache

(defvar *usage-sink* nil
  "Bound to a USAGE struct to accumulate LLM token/cost usage; else NIL.")

(defun fold-usage (dst parsed)
  "Add PARSED's OpenAI/OpenRouter `usage` block into the DST usage struct,
   counting one call. Also captures prefix-cache hits — providers report
   them differently: OpenAI/OpenRouter as usage.prompt_tokens_details.
   cached_tokens, Anthropic as usage.cache_read_input_tokens. Returns DST;
   a NIL DST is a no-op."
  (when (and dst parsed (hash-table-p parsed))
    (incf (usage-calls dst))
    (let ((u (gethash "usage" parsed)))
      (when (hash-table-p u)
        (flet ((num (k &optional (h u)) (let ((v (and (hash-table-p h) (gethash k h))))
                                          (if (realp v) v 0))))
          (incf (usage-prompt-tokens dst) (num "prompt_tokens"))
          (incf (usage-completion-tokens dst) (num "completion_tokens"))
          (incf (usage-cost-usd dst) (coerce (num "cost") 'double-float))
          (incf (usage-cached-tokens dst)
                (+ (num "cached_tokens" (gethash "prompt_tokens_details" u))
                   (num "cache_read_input_tokens")))))))
  dst)

(defun usage-incf (dst src)
  "Add SRC usage into DST destructively (for rolling nested/subagent usage
   up into a parent). Returns DST; NIL operands no-op."
  (when (and dst src)
    (incf (usage-calls dst)             (usage-calls src))
    (incf (usage-prompt-tokens dst)     (usage-prompt-tokens src))
    (incf (usage-completion-tokens dst) (usage-completion-tokens src))
    (incf (usage-cost-usd dst)          (usage-cost-usd src))
    (incf (usage-cached-tokens dst)     (usage-cached-tokens src)))
  dst)

(defun usage-summary (u)
  "Human-readable one-liner for a usage struct."
  (if u
      (format nil "~,3F¢, ~A prompt~@[ (~A cached)~]/~A completion tok, ~A call~:P"
              (* 100 (usage-cost-usd u)) (usage-prompt-tokens u)
              (when (plusp (usage-cached-tokens u)) (usage-cached-tokens u))
              (usage-completion-tokens u) (usage-calls u))
      "no usage"))

(defun note-usage (parsed)
  "Fold PARSED's usage block into *USAGE-SINK* (if bound)."
  (fold-usage *usage-sink* parsed))

(defmacro with-usage-accounting ((var) &body body)
  "Bind VAR to a fresh USAGE struct and *USAGE-SINK* to it for BODY's
   dynamic extent, so LLM calls within BODY accumulate into VAR."
  `(let* ((,var (make-usage)) (*usage-sink* ,var))
     ,@body))

(defun extract-json (text)
  "Locate the first balanced JSON object in TEXT and parse it. Tolerates
   leading/trailing prose and ```json ... ``` markdown fences. Returns
   the parsed json-simple hash-table, or NIL if no valid object is found."
  (when (stringp text)
    (let ((start (position #\{ text)))
      (when start
        (let ((depth 0) (in-string nil) (escape nil))
          (loop for i from start below (length text)
                for c = (char text i)
                do (cond (escape (setf escape nil))
                         ((char= c #\\) (setf escape t))
                         ((char= c #\") (setf in-string (not in-string)))
                         (in-string)
                         ((char= c #\{) (incf depth))
                         ((char= c #\}) (decf depth)
                          (when (zerop depth)
                            (return (handler-case
                                        (json:parse
                                         (subseq text start (1+ i)))
                                      (error () nil))))))))))))

(defparameter *llm-max-retries* 4
  "Retries for a TRANSIENT LLM HTTP failure (429 rate-limit, 5xx, network/
   timeout) before giving up. Backoff is exponential. A provider 429 must not
   kill a whole scan/report — it usually clears within a few seconds.")

(defvar *sealed-post* nil
  "When bound to a function, %LLM-POST hands the request to it instead of
   sending it in the clear.  The function is called (URL BODY HEADERS) and
   returns the response body as a string.  This is how confidential :PPQ seals
   every request to a verified enclave without the call sites knowing: the
   body is built exactly as for any other backend, and the transport is what
   changes.  NIL means an ordinary HTTP POST.")

(defun %llm-post (url body headers)
  "POST a chat completion with retry + exponential backoff on transient
   failures. Retries HTTP 429 and 5xx and network/timeout errors; resignals
   non-transient failures (4xx other than 429) immediately.

   When *SEALED-POST* is bound, the request goes through it (EHBP-sealed to an
   attested enclave) and the retry policy is the sealer's business — it has
   its own, and it must not be retried here, because a refusal from the
   sealer is a trust decision, not a transient network fault."
  (if *sealed-post*
      (funcall *sealed-post* url body headers)
      (progn
        ;; Belt and braces: *CONFIDENTIAL* is a promise that the body is
        ;; sealed, so a plaintext POST while it is set is a bug in the caller,
        ;; not a request to send.  Fail here rather than leak the prompt.
        (when *confidential*
          (error "confidential mode is on but no sealer is installed — refusing to send ~
                  a plaintext request to ~A" url))
        (let ((attempt 0))
        (loop
          (handler-case
              (return (http:post url :content body :headers headers
                                     :read-timeout *llm-read-timeout*))
            (http:http-request-failed (e)
              (let ((code (ignore-errors (http:response-status e))))
                (if (and (or (eql code 429) (and (integerp code) (>= code 500)))
                         (< attempt *llm-max-retries*))
                    (progn (sleep (min 30 (* 3 (expt 2 attempt)))) (incf attempt))
                    (error e))))           ; 4xx (not 429) etc. — not recoverable
            (error (e)                      ; network / read-timeout — transient
              (if (< attempt *llm-max-retries*)
                  (progn (sleep (min 30 (* 3 (expt 2 attempt)))) (incf attempt))
                  (error e)))))))))

(defun post-chat (url body headers &key want-stream)
  "The single transport for a chat completion, so that EVERY caller — the
   engine's blocking and streaming paths as well as LLM-CHAT — goes through
   the same door.  When *SEALED-POST* is bound (confidential :PPQ)
   the body is EHBP-sealed to a locally verified enclave and the response is
   decrypted; otherwise it is an ordinary HTTP POST.

   WANT-STREAM asks for the raw response stream (SSE).  A sealer cannot
   stream — the whole body is one encrypted frame — so asking for a stream
   under a sealer is a programming error, not something to paper over."
  (if *sealed-post*
      (progn
        (when want-stream
          (error "post-chat: the sealed (confidential :ppq) transport cannot stream; \
call with :want-stream nil"))
        (funcall *sealed-post* url body headers))
      (http:post url :content body :headers headers
                     :read-timeout *llm-read-timeout*
                     :want-stream want-stream)))

(defun llm-chat (prompt &key system
                             (max-tokens *llm-default-max-tokens*)
                             (temperature 0.0)
                             (think nil think-p)
                             (effort nil effort-p)
                             extra)
  "POST a chat completion. Backend-aware: on :LLAMA includes
   chat_template_kwargs.enable_thinking; on :OPENROUTER sends
   model + Authorization. Returns (values content reasoning timings
   raw-parsed).

   EFFORT overrides *LLM-EFFORT* for this call. THINK is the older switch
   the side-callers (summarizer, safefetch) use: NIL means :OFF on llama
   as it always did, but only :LOW on OpenRouter — these are cheap utility
   calls, and :OFF would 400 on a model that requires reasoning."
  (let* ((msgs (let ((acc '()))
                 (when system
                   (push (ht "role" "system" "content" system) acc))
                 (push (ht "role" "user" "content" prompt) acc)
                 (coerce (nreverse acc) 'vector)))
         (body-ht (ht "messages" msgs
                      "max_tokens" max-tokens
                      "temperature" temperature)))
    (apply-reasoning body-ht
                     (cond (effort-p effort)
                           (think-p (cond (think :medium)
                                          ((eq *llm-backend* :llama) :off)
                                          (t :low)))
                           (t *llm-effort*)))
    (when *llm-model*
      (setf (gethash "model" body-ht) *llm-model*))
    ;; OpenRouter only reports real USD cost when asked; harmless to llama.
    ;; Not sent to confidential :PPQ — the enclave's OpenAI dialect is not
    ;; OpenRouter's, and an unknown field is a 400 we would have to debug.
    (when (member *llm-backend* '(:openrouter :routstr))
      (setf (gethash "usage" body-ht) (ht "include" t)))
    (loop for (k v) on extra by #'cddr
          do (setf (gethash (string-downcase (string k)) body-ht) v))
    (let* ((body (with-output-to-string (s)
                   (json:stringify body-ht :stream s)))
           (headers (cond
                      (*llm-auth-token*
                       `(("Content-Type" . "application/json")
                         ("Authorization" . ,(concatenate 'string
                                                          "Bearer "
                                                          *llm-auth-token*))))
                      (t '(("Content-Type" . "application/json")))))
           (resp (%llm-post *llm-url* body headers))
           (parsed (json:parse resp))
           (choices (gethash "choices" parsed))
           (msg (and (vectorp choices) (plusp (length choices))
                     (gethash "message" (aref choices 0)))))
      (note-usage parsed)
      ;; reasoning_content is the local llama.cpp key; OpenRouter exposes
      ;; the same trace as "reasoning". Pick whichever is non-empty.
      (values (and msg (gethash "content" msg))
              (and msg (or (let ((v (gethash "reasoning_content" msg)))
                             (and v (stringp v) (plusp (length v)) v))
                           (gethash "reasoning" msg)))
              (gethash "timings" parsed)
              parsed))))

(defun read-token-file (path)
  (handler-case
      (with-open-file (s path)
        (string-trim '(#\Space #\Newline #\Return #\Tab)
                     (read-line s nil "")))
    (error () nil)))

(defun openrouter-credits (&optional (token-file (merge-pathnames
                                                  ".operandi/openrouter.token"
                                                  (user-homedir-pathname))))
  "Query OpenRouter for account credits. Returns (values remaining-usd
   total-usd usage-usd), or NIL on failure."
  (let ((token (read-token-file token-file)))
    (when (and token (plusp (length token)))
      (handler-case
          (let* ((resp (http:get "https://openrouter.ai/api/v1/credits"
                                 :headers `(("Authorization" . ,(concatenate 'string "Bearer " token)))
                                 :read-timeout 15))
                 (d (gethash "data" (json:parse resp)))
                 (total (gethash "total_credits" d))
                 (usage (gethash "total_usage" d)))
            (values (- total usage) total usage))
        (error () nil)))))

(defun set-backend-openrouter! (&key (model "minimax/minimax-m2.7")
                            (token-file (merge-pathnames
                                         ".operandi/openrouter.token"
                                         (user-homedir-pathname))))
  "Switch the LLM client to OpenRouter. Reads the bearer token from
   TOKEN-FILE (default ~/.operandi/openrouter.token, mode 600).
   Returns T on success, NIL if the token file is missing or empty."
  (let ((token (read-token-file token-file)))
    (cond
      ((or (null token) (zerop (length token)))
       (format *error-output*
               "~&set-backend-openrouter!: token file ~A missing or empty~%"
               token-file)
       nil)
      (t
       (setf *llm-backend*    :openrouter
             *llm-url*        "https://openrouter.ai/api/v1/chat/completions"
             *llm-auth-token* token
             *llm-model*      model
             *llm-read-timeout* 240
             *sealed-post*    nil)
       t))))

(defparameter *routstr-url* "https://routstr.cypherpunk.today/v1/chat/completions"
  "Chat endpoint of one Routstr provider NODE. Routstr is a network of
   independent OpenAI-compatible nodes that each validate the Cashu token
   and serve their own model ids (hyphenated, e.g. gemini-3-6-flash).
   api.routstr.com is only the provider directory, not a chat endpoint.
   Any live node works; set this to choose another.")

(defun routstr-chat-url (url)
  "The chat-completions endpoint of a Routstr node, given either the node root
   (https://node.example) or a full endpoint URL.  Nodes are advertised by root
   URL, but *ROUTSTR-URL* has always carried the path, so accept both rather
   than making the caller remember which form this argument wants."
  (let* ((u (string-right-trim "/" url))
         (u (if (uiop:string-suffix-p u "/v1/chat/completions")
                (subseq u 0 (- (length u) (length "/v1/chat/completions")))
                u))
         (u (if (uiop:string-suffix-p u "/v1")
                (subseq u 0 (- (length u) (length "/v1")))
                u)))
    (format nil "~A/v1/chat/completions" u)))

(defun set-backend-routstr! (&key (model "gemini-3-6-flash")
                             confidential
                             (url *routstr-url*)
                             (token-file (merge-pathnames
                                          ".operandi/routstr.token"
                                          (user-homedir-pathname))))
  "Switch the LLM client to Routstr. Reads the bearer token (a Cashu eCash
   token or an sk- API key from chat.routstr.com) from TOKEN-FILE (default
   ~/.operandi/routstr.token, mode 600). Returns T on success, NIL if the
   token file is missing or empty.

   With CONFIDENTIAL (or with *CONFIDENTIAL* already set by --confidential),
   MODEL must be a `tinfoil-*` id and every request is EHBP-sealed to the HPKE
   key of an AMD-attested Tinfoil enclave, verified locally against the pinned
   AMD ARK before the first byte is sent.  The node then relays the sealed body
   blind.  Any other model is refused HERE rather than at request time, so a
   misconfiguration fails at startup instead of silently sending plaintext."
  (let ((token (read-token-file token-file))
        ;; --confidential may have been given BEFORE --routstr, so the session
        ;; flag wins over the keyword: once confidential mode is on, choosing a
        ;; backend must not silently turn it off.
        (conf (or confidential *confidential*)))
    (cond
      ((or (null token) (zerop (length token)))
       (format *error-output*
               "~&set-backend-routstr!: token file ~A missing or empty~%"
               token-file)
       nil)
      ((and conf (operandi.ppq:routstr-confidential-p model))
       (format *error-output*
               "~&operandi: --confidential cannot be satisfied by a Routstr node.  \
~s is relayed blind to a real Tinfoil enclave, but the key we can seal to is the \
ROUTER's (inference.tinfoil.sh), and the router re-seals to the per-model \
container — a second hop we cannot verify from here.  The request is encrypted, \
not end-to-end attested.  Use --tinfoil ~a, or --confidential --ppq \
private/<model>, or drop --confidential to use the node as an ordinary provider.~%"
               model (operandi.ppq:routstr-wire-model model))
       nil)
      (t
       (setf *llm-backend*    :routstr
             *llm-url*        (routstr-chat-url url)
             *llm-auth-token* token
             *llm-model*      model
             *llm-read-timeout* 240
             *confidential*   conf
             ;; A tinfoil-* model has NO plaintext mode: the node relays EHBP
             ;; bodies to the enclave and 404s anything else, so the sealer is
             ;; installed whenever the model is one of those.  This is
             ;; ENCRYPTION, not attestation: we seal to the ROUTER's key
             ;; (inference.tinfoil.sh), and the router re-seals to the per-model
             ;; container, so the node and the router both sit inside the trust
             ;; set.  That is why --confidential refuses :routstr outright (see
             ;; CONFIDENTIAL-BACKEND-P) while the model stays usable without it.
             *sealed-post*    (and (operandi.ppq:routstr-confidential-p model)
                                   (lambda (url body headers)
                                     (operandi.ppq:sealed-post
                                      url body headers
                                      :base operandi.ppq:*tinfoil-base*
                                      :attestation-url operandi.ppq:*tinfoil-attestation-url*
                                      :model (operandi.ppq:routstr-wire-model model)
                                      :model-header "X-Routstr-Model"))))
       t))))

(defun set-backend-tinfoil! (&key (model "gpt-oss-120b")
                                  (base operandi.ppq:*tinfoil-base*)
                                  (attestation-url operandi.ppq:*tinfoil-attestation-url*)
                                  (token-file operandi.ppq:*tinfoil-token-file*))
  "Switch the LLM client to Tinfoil's OWN enclave (inference.tinfoil.sh).

   There is no plaintext mode here and no --confidential to ask for: this
   backend exists only as the sealed one.  Every request is EHBP-sealed to the
   HPKE key of an AMD SEV-SNP enclave verified locally against the pinned AMD
   ARK, and a response without an Ehbp-Response-Nonce is refused rather than
   parsed.  MODEL is a bare Tinfoil id (gpt-oss-120b, glm-5-3, kimi-k3, …).

   Returns T on success, NIL if the token file is missing or empty."
  (let ((token (read-token-file token-file)))
    (cond
      ((or (null token) (zerop (length token)))
       (format *error-output* "~&set-backend-tinfoil!: token file ~A missing or empty~%"
               token-file)
       nil)
      (t
       (setf *llm-backend*    :tinfoil
             *llm-url*        (format nil "~A/v1/chat/completions" base)
             *llm-auth-token* token
             *llm-model*      model
             *llm-read-timeout* 240
             ;; Sealed always, and *CONFIDENTIAL* is set with it so that the
             ;; plaintext guard in %LLM-POST holds even if the sealer were
             ;; somehow cleared: on this backend there is no unsealed request.
             *confidential*   t
             *sealed-post*    (lambda (url body headers)
                                (operandi.ppq:sealed-post
                                 url body headers :base base
                                 :attestation-url attestation-url)))
       t))))

(defun set-backend-ppq! (&key (model "private/gpt-oss-120b")
                              confidential
                              (base operandi.ppq:*ppq-base*)
                              (private-base operandi.ppq:*ppq-private-base*)
                              (token-file operandi.ppq:*ppq-token-file*))
  "Switch the LLM client to PPQ.

   Without CONFIDENTIAL this is an ordinary hosted provider: MODEL is any PPQ
   model id (gpt-6.1-sol, claude-sonnet-5.5, …) and requests go out in the
   clear to BASE, exactly as they would to OpenRouter.

   With CONFIDENTIAL, MODEL must be a `private/<name>` id and every request is
   EHBP-sealed to the HPKE key of an AMD-attested Tinfoil enclave, verified
   locally against the pinned AMD ARK before the first byte is sent.  A
   non-private model is refused HERE rather than at request time, so a
   misconfiguration fails at startup instead of silently sending plaintext.

   Returns T on success, NIL if the token file is missing or empty."
  (let ((token (read-token-file token-file))
        ;; --confidential may have been given BEFORE --ppq (the natural order
        ;; is `--confidential --ppq private/x`), so the session flag wins over
        ;; the keyword: once confidential mode is on, choosing a backend must
        ;; not silently turn it off.
        (conf (or confidential *confidential*)))
    (cond
      ((or (null token) (zerop (length token)))
       (format *error-output* "~&set-backend-ppq!: token file ~A missing or empty~%"
               token-file)
       nil)
      ((and conf (not (operandi.ppq:confidential-p model)))
       (format *error-output*
               "~&set-backend-ppq!: ~s is not a private/* model — Path B is not allowed under --confidential~%"
               model)
       nil)
      (t
       (setf *llm-backend*    :ppq
             *llm-url*        (format nil "~A/v1/chat/completions"
                                      (if conf private-base base))
             *llm-auth-token* token
             *llm-model*      model
             *llm-read-timeout* 240
             *confidential*   conf
             ;; The sealer is what makes confidential mode what it is.  Setting
             ;; the mode WITHOUT it would send the body in the clear — PPQ
             ;; answers 400 "the Ehbp-Encapsulated-Key header is missing", so
             ;; the failure is loud, but the point is that the two must never
             ;; come apart.  (WITH-PPQ binds it dynamically; this is the
             ;; global/CLI path, so it is setf'd.)
             *sealed-post*    (and conf
                                    (lambda (url body headers)
                                      (operandi.ppq:sealed-post
                                       url body headers :base private-base
                                       :model (operandi.ppq:ppq-wire-model model)))))
       t))))

(defmacro with-ppq ((&key (model "private/gpt-oss-120b")
                          confidential
                          (base operandi.ppq:*ppq-base*)
                          (private-base operandi.ppq:*ppq-private-base*)
                          (token-file operandi.ppq:*ppq-token-file*))
                    &body body)
  "Run BODY with the LLM client switched to :PPQ.

   With CONFIDENTIAL, MODEL must be a private/* id and the exchange is sealed
   to a locally verified enclave; if the token is missing or MODEL is not
   private/*, this SIGNALS — it does not fall through to the current backend,
   because a silent downgrade to plaintext is exactly what confidential mode
   exists to prevent.  Without CONFIDENTIAL it is an ordinary hosted call."
  (let ((tok (gensym)) (tf (gensym)) (md (gensym)))
    `(let* ((,tf ,token-file)
            (,md ,model)
            (,tok (read-token-file ,tf)))
       (unless (and ,tok (plusp (length ,tok)))
         (error "with-ppq: no PPQ API key at ~A" ,tf))
       ,@(when confidential
           `((unless (operandi.ppq:confidential-p ,md)
               (error "with-ppq: ~s is not a private/* model — Path B is not allowed under :confidential t"
                      ,md))))
       (let ((*llm-backend* :ppq)
             (*llm-url* (format nil "~A/v1/chat/completions"
                                (if ,confidential ,private-base ,base)))
             (*llm-auth-token* ,tok)
             (*llm-model* ,md)
             (*llm-read-timeout* 240)
             (*confidential* (and ,confidential t))
             (*sealed-post* (and ,confidential
                                 (lambda (url body headers)
                                   (operandi.ppq:sealed-post
                                    url body headers :base ,private-base
                                    :model (operandi.ppq:ppq-wire-model ,md))))))
         ,@body))))

(defun set-backend-llama! (&key (url "http://127.0.0.1:8081/v1/chat/completions"))
  "Switch the LLM client back to the local llama.cpp server."
  (setf *llm-backend*    :llama
        *llm-url*        url
        *llm-auth-token* nil
        *llm-model*      nil
        *llm-read-timeout* 120
        ;; Leaving a sealer installed would EHBP-seal a request to a backend
        ;; that never asked for it.  The sealer belongs to confidential :PPQ
        ;; alone, so every other backend switch clears it.
        *sealed-post*    nil
        *confidential*   nil)
  t)

(defmacro with-llama (&body body)
  "Run BODY with the LLM client temporarily switched to local llama.cpp,
   regardless of the global default. Useful for high-volume per-call
   work (paper-trader market evals) that shouldn't burn paid API
   credits even when operandi's cron tasks are running on a frontier
   backend."
  `(let ((*llm-backend* :llama)
         (*llm-url* "http://127.0.0.1:8081/v1/chat/completions")
         (*llm-auth-token* nil)
         (*llm-model* nil)
         (*llm-read-timeout* 120)
         (*sealed-post* nil))
     ,@body))

(defmacro with-openrouter ((&key (model "anthropic/claude-sonnet-4-5")
                                  (token-file (merge-pathnames
                                               ".operandi/openrouter.token"
                                               (user-homedir-pathname))))
                            &body body)
  "Run BODY with the LLM client temporarily switched to OpenRouter."
  (let ((tok (gensym))
        (tf  (gensym)))
    `(let* ((,tf ,token-file)
            (,tok (read-token-file ,tf)))
       (cond
         ((or (null ,tok) (zerop (length ,tok)))
          (format *error-output*
                  "~&with-openrouter: token file ~A missing — falling through to current backend~%"
                  ,tf)
          ,@body)
         (t
          (let ((*llm-backend* :openrouter)
                (*llm-url* "https://openrouter.ai/api/v1/chat/completions")
                (*llm-auth-token* ,tok)
                (*llm-model* ,model)
                (*llm-read-timeout* 240)
                (*sealed-post* nil))
            ,@body))))))

(defmacro with-routstr ((&key (model "gemini-3-6-flash")
                              (url *routstr-url*)
                              (token-file (merge-pathnames
                                           ".operandi/routstr.token"
                                           (user-homedir-pathname))))
                        &body body)
  "Run BODY with the LLM client temporarily switched to Routstr.  URL names the
   node; a `tinfoil-*` MODEL on a node that carries Tinfoil is relayed blind to
   the attested enclave, but this macro does NOT turn on confidential mode —
   that is --confidential's job, and it is a property of the session."
  (let ((tok (gensym))
        (tf  (gensym)))
    `(let* ((,tf ,token-file)
            (,tok (read-token-file ,tf)))
       (cond
         ((or (null ,tok) (zerop (length ,tok)))
          (format *error-output*
                  "~&with-routstr: token file ~A missing — falling through to current backend~%"
                  ,tf)
          ,@body)
         (t
          (let ((*llm-backend* :routstr)
                (*llm-url* (routstr-chat-url ,url))
                (*llm-auth-token* ,tok)
                (*llm-model* ,model)
                (*llm-read-timeout* 240)
                (*sealed-post* nil))
            ,@body))))))
