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
           #:*llama-url* #:llama-chat-url #:llama-ask-model #:llama-resolve-model
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
           #:fold-usage #:usage-incf #:usage-summary #:ask-usage-cost-p))

(in-package #:operandi.llm)

(defparameter *llama-url* "http://localhost:9931/v1/chat/completions"
  "The local llama.cpp server.  llama.cpp's own default port is 8080, but the
   build we run listens on 9931; --llama (and the \"llama\" config model) point
   here, and --llama URL overrides it for one run.")

;;; The live backend state is DEFVAR, not DEFPARAMETER, on purpose.  These are
;;; the knobs a running session turns — /model, --llama, set-backend-*! — and
;;; DEFPARAMETER would RESET them every time this file is loaded into a live
;;; image.  That is not hypothetical: re-loading llm.lisp to pick up an edit
;;; mid-session silently reverted the backend to :llama with a NIL model, and
;;; the next turn went to the local server with no model field and 400'd
;;; ("model name is missing from the request").  DEFVAR leaves a bound symbol
;;; alone, so a reload is now a no-op for state and only replaces the code.
(defvar *llm-url* *llama-url*)
(defvar *llm-default-max-tokens* 256)
(defvar *llm-read-timeout* 120)

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

(defparameter *ppq-fee-factor* 1.055d0
  "PPQ's usage.cost is the UPSTREAM price; what PPQ charges (its
   /queries/history price_in_usd, the balance drop) is this multiple of it.
   Measured 2026-10-16: 1.0550 on four calls across three upstream providers
   and two models. Re-measure if PPQ's pricing changes.")

(defvar *llm-backend* :llama
  ":LLAMA = local llama.cpp at *llm-url* (default).
   :OPENROUTER = OpenRouter API; *llm-url*, *llm-auth-token*, and
   *llm-model* must be set. Use USE-OPENROUTER to flip.
   :ROUTSTR = Routstr (api.routstr.com/v1, OpenAI-compatible, Cashu-paid).
   Same request shape as OpenRouter; the bearer token is a Cashu token or
   an sk- API key.
   :PPQ = PPQ (ppq.ai), OpenAI-compatible, paid in Bitcoin/Lightning.  By
   default it is an ordinary hosted provider: *llm-model* is any PPQ model id
   and the request goes out in the clear, exactly like OpenRouter.
   With *CONFIDENTIAL* bound true (the --confidential-tinfoil flag) it becomes Path A
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
   --confidential-tinfoil flag (and by WITH-PPQ :CONFIDENTIAL T).

   This is a property of the SESSION, not of the backend: it is what turns
   :PPQ from \"a hosted provider\" into \"Path A only, no fallback\".  It is
   deliberately separate from the backend choice so that a caller can say
   \"I do not care which provider, but I do care that it is attested\" and
   have that be a checkable claim rather than a naming convention.")

(defun confidential-backend-p (&optional (backend *llm-backend*) (model *llm-model*))
  "True when BACKEND can satisfy *CONFIDENTIAL* for MODEL.

   THE CRITERION.  --confidential-tinfoil is satisfied iff a cleartext leak would
   require breaking the hardware TEE the request is sealed to, or leaking the
   silicon vendor's key.  It is NOT a claim about how many parties sit on the
   path, and it is NOT a claim that every hop is independently verified.  A
   party that can only drop, delay, or observe the metadata of an opaque body
   is not a party that can leak it, so its presence does not defeat the claim.

   HOW EACH BACKEND MEETS IT.  In all three cases the client seals to a key
   that is bound into an AMD SEV-SNP report we verify locally against the pinned
   AMD ARK, so the body can only be opened by an enclave running the measured
   image — a leak requires breaking SEV-SNP or AMD's key.

     :PPQ (Path A)  seal to api.ppq.ai/private's key.  That key is byte-identical
                    to inference.tinfoil.sh's (b92b915f…): PPQ's private base IS
                    the Tinfoil router, reached through PPQ's billing.
     :TINFOIL       seal to inference.tinfoil.sh's key directly.
     :ROUTSTR       seal to the same router key, for a `tinfoil-*` model.  The
                    node is a BLIND RELAY and we have proved it: sealing to the
                    model container's key instead makes the node answer 422
                    \"failed to read decrypted request body\", so it cannot open
                    what we send.  It forwards opaque bytes and relays the sealed
                    answer.  It can drop the request or see the model and
                    timing; it cannot read the body or forge the reply, because
                    the response nonce is derived from our ephemeral key.

   THE ONE UNVERIFIED HOP, WHICH ALL THREE SHARE.  The router terminates our
   sealed body and re-seals to a per-model container (the response names it:
   tinfoil-enclave: <model>-infN-M.tinfoil.containers.tinfoil.dev).  That
   container is Intel TDX, and we cannot verify its quote against a pinned root
   the way we verify the SEV-SNP one — ATC refuses it outright (\"only supports
   sev-snp-guest/v2\").  So the container hop rests on the router's word.  This
   is a real limit, but it is a limit of the TINFOIL ROUTER, not of Routstr:
   --tinfoil and --confidential-tinfoil --ppq private/… go through the same router and
   the same re-seal.  Refusing Routstr for it while accepting the other two
   would be incoherent.  What the router's word buys an attacker is bounded by
   the same hardware: to turn it into a cleartext leak they must still break the
   TDX container, or the SEV-SNP router, or AMD's key.

   :OPENROUTER and :LLAMA cannot seal at all, so --confidential-tinfoil with either is a
   configuration error, not a silent downgrade."
  (case backend
    ((:ppq :tinfoil) t)
    (:routstr (operandi.ppq:routstr-confidential-p model))
    (t nil)))

(defun enable-confidential! ()
  "Turn on confidential mode for the current session (the --confidential-tinfoil flag).

   If the backend is already :PPQ or :ROUTSTR, this also (re)installs the sealer
   and re-checks that the current model is one that can be served confidentially
   — so `--ppq X --confidential-tinfoil` and `--confidential-tinfoil --ppq X` behave
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
     (unless (operandi.ppq:routstr-confidential-p *llm-model*)
       (error "confidential mode: ~s is not a tinfoil-* model — a Routstr node \
terminates every other model itself, so it is not sealed to an enclave at all. \
Use --routstr tinfoil-<model> on a node that carries Tinfoil, or drop \
--confidential-tinfoil."
              *llm-model*))
     ;; *LLM-URL* already names the node (SET-BACKEND-ROUTSTR! set it); only the
     ;; transport changes here, so a node chosen on the command line survives.
     ;; The node is a blind relay: it cannot open this body (proved — sealing to
     ;; the container key instead makes it answer 422), so it is on the path but
     ;; not in the trust set.
     (setf *sealed-post* (lambda (url body headers)
                           (operandi.ppq:sealed-post
                            url body headers
                            :base operandi.ppq:*tinfoil-base*
                            :attestation-url operandi.ppq:*tinfoil-attestation-url*
                            :model (operandi.ppq:routstr-wire-model *llm-model*)
                            :model-header "X-Routstr-Model")))))
  t)

(defun require-confidential! ()
  "Signal unless the current backend can honour *CONFIDENTIAL*.  Called at
   startup so that `--confidential-tinfoil --openrouter ...` fails immediately and
   loudly instead of quietly sending plaintext for the whole session."
  (when (and *confidential* (not (confidential-backend-p)))
    (error "confidential mode is not available on the ~(~a~) backend~@[ for model ~s~]: \
--confidential-tinfoil requires that a cleartext leak would need the hardware TEE broken \
or the silicon vendor's key leaked. Use --confidential-tinfoil --ppq private/<model>, \
--tinfoil <model>, or --confidential-tinfoil --routstr tinfoil-<model> on a node that \
carries Tinfoil, or drop --confidential-tinfoil."
           *llm-backend* (and (eq *llm-backend* :routstr) *llm-model*)))
  t)

(defvar *llm-effort* nil
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

(defvar *llm-auth-token* nil
  "Bearer token for the LLM endpoint, or NIL. Required for openrouter.")

(defvar *llm-model* nil
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

(defun ask-usage-cost-p (&optional (backend *llm-backend*))
  "Whether a request on BACKEND carries {\"usage\":{\"include\":true}}.
   OpenRouter and Routstr only report USD cost when asked. Hosted :PPQ too,
   and there asking also ROUTES: PPQ skips the upstream providers that reject
   the field (its ppq-routing-receipt says unsupported_field), so every answer
   comes from one that reports cost -- on purpose: we would rather lose a
   provider than a run's spend. Not confidential :PPQ: the enclave's OpenAI
   dialect does not know the field and 400s on it."
  (or (member backend '(:openrouter :routstr))
      (and (eq backend :ppq) (not *confidential*))))

(defun fold-usage (dst parsed)
  "Add PARSED's OpenAI/OpenRouter `usage` block into the DST usage struct,
   counting one call. Also captures prefix-cache hits — providers report
   them differently: OpenAI/OpenRouter as usage.prompt_tokens_details.
   cached_tokens, Anthropic as usage.cache_read_input_tokens. Returns DST;
   a NIL DST is a no-op.

   When the provider reports tokens but NO cost (PPQ's sealed Path A enclave
   does exactly this), price the turn from PPQ's catalogue instead of folding
   a 0 — otherwise a confidential run shows 0.000¢ forever."
  (when (and dst parsed (hash-table-p parsed))
    (incf (usage-calls dst))
    (let ((u (gethash "usage" parsed)))
      (when (hash-table-p u)
        (flet ((num (k &optional (h u)) (let ((v (and (hash-table-p h) (gethash k h))))
                                          (if (realp v) v 0))))
          (let* ((pt (num "prompt_tokens"))
                 (ct (num "completion_tokens"))
                 (cached (+ (num "cached_tokens" (gethash "prompt_tokens_details" u))
                            (num "cache_read_input_tokens")))
                 (cost (coerce (num "cost") 'double-float)))
            (incf (usage-prompt-tokens dst) pt)
            (incf (usage-completion-tokens dst) ct)
            (incf (usage-cached-tokens dst) cached)
            ;; PPQ's usage.cost is the UPSTREAM price; what it charges is this multiple
            (when (eq *llm-backend* :ppq)
              (setf cost (* cost *ppq-fee-factor*)))
            (when (and (zerop cost) (eq *llm-backend* :ppq) *llm-model*)
              (setf cost (or (operandi.ppq:ppq-price-usd *llm-model* pt ct
                                                         :cached-tokens cached)
                             0d0)))
            (incf (usage-cost-usd dst) cost))))))
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

(defvar *llm-max-retries* 4
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
    ;; OpenRouter, Routstr and PPQ only report real USD cost when asked (see
    ;; ASK-USAGE-COST-P: confidential PPQ does not get the field).
    (when (ask-usage-cost-p)
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

   With CONFIDENTIAL (or with *CONFIDENTIAL* already set by --confidential-tinfoil),
   MODEL must be a `tinfoil-*` id and every request is EHBP-sealed to the HPKE
   key of an AMD-attested Tinfoil enclave, verified locally against the pinned
   AMD ARK before the first byte is sent.  The node then relays the sealed body
   blind.  Any other model is refused HERE rather than at request time, so a
   misconfiguration fails at startup instead of silently sending plaintext."
  (let ((token (read-token-file token-file))
        ;; --confidential-tinfoil may have been given BEFORE --routstr, so the session
        ;; flag wins over the keyword: once confidential mode is on, choosing a
        ;; backend must not silently turn it off.
        (conf (or confidential *confidential*)))
    (cond
      ((or (null token) (zerop (length token)))
       (format *error-output*
               "~&set-backend-routstr!: token file ~A missing or empty~%"
               token-file)
       nil)
      ((and conf (not (operandi.ppq:routstr-confidential-p model)))
       (format *error-output*
               "~&operandi: ~s is not a tinfoil-* model — a Routstr node \
terminates every other model itself, so it is not sealed to an enclave at all~%"
               model)
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
             ;; installed whenever the model is one of those.  The node is a
             ;; BLIND RELAY — sealing to the container's key instead makes it
             ;; answer 422, so it cannot open this body — and the key we seal to
             ;; is the router's, verified locally against the pinned AMD ARK.
             ;; --confidential-tinfoil only decides whether an unattestable transport is
             ;; an ERROR; see CONFIDENTIAL-BACKEND-P for the criterion.
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

   There is no plaintext mode here and no --confidential-tinfoil to ask for: this
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
        ;; --confidential-tinfoil may have been given BEFORE --ppq (the natural order
        ;; is `--confidential-tinfoil --ppq private/x`), so the session flag wins over
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
               "~&set-backend-ppq!: ~s is not a private/* model — Path B is not allowed under --confidential-tinfoil~%"
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

(defun llama-chat-url (spec)
  "Turn a --llama argument into a chat-completions URL.  SPEC is
   [proto://][host][:port][/path] — the address of the server, not a model
   name: a llama.cpp server usually has exactly one model loaded, so there is
   nothing to name (use -m for the rare multi-model server).  At least one
   component is required, so an empty argument SIGNALS rather than quietly
   falling back to the default — a bare --llama is a mistake, not a request
   for localhost.  A missing scheme means http, a missing host means
   localhost, and a missing path means /v1/chat/completions.

   A PORT MUST BE COLON-PREFIXED: \":9931\" is localhost on 9931.  A bare
   number is NOT read as a port — it SIGNALS, because a bare number is a
   forgotten colon far more often than it is a hostname, and silently
   resolving \"9931\" to localhost:9931 would hide the typo.  A path is kept as
   given, so a full endpoint passes through unchanged."
  (let* ((s (string-trim " " (string spec)))
         (scheme-end (search "://" s))
         (scheme (if scheme-end (subseq s 0 (+ scheme-end 3)) "http://"))
         (rest (if scheme-end (subseq s (+ scheme-end 3)) s))
         (slash (position #\/ rest))
         (authority (if slash (subseq rest 0 slash) rest))
         (path (if slash (subseq rest slash) "")))
    (when (and (plusp (length authority)) (every #'digit-char-p authority))
      (error "a port must be colon-prefixed: --llama :~A, not ~A" authority authority))
    (when (and (plusp (length authority)) (char= (char authority (1- (length authority))) #\:))
      (error "a port must be colon-prefixed and non-empty: --llama :9931, not ~A" authority))
    (when (and (string= authority "") (or (string= path "") (string= path "/")))
      (error "--llama needs an address: [proto://][host][:port][/path] (e.g. --llama :9931)"))
    (let ((authority (cond ((string= authority "") "localhost")
                           ((char= (char authority 0) #\:)
                            (concatenate 'string "localhost" authority))
                           (t authority))))
      (if (and (plusp (length path)) (not (string= path "/")))
          (format nil "~A~A~A" scheme authority path)
          (format nil "~A~A/v1/chat/completions" scheme authority)))))

;;; ---- asking a llama.cpp server what it serves ------------------------------
;;; A single-model llama.cpp server needs no model field at all, but its
;;; ROUTER mode (several models behind one port, loaded on demand) 400s a
;;; request that omits one.  So when no model was named we ask the server:
;;; GET <base>/v1/models, and pick from what comes back.

(defun llama-models-url (chat-url)
  "The /v1/models URL for a chat-completions URL — the sibling endpoint, so a
   custom path (a proxy prefix) is preserved."
  (let ((i (search "/v1/chat/completions" chat-url)))
    (if i (concatenate 'string (subseq chat-url 0 i) "/v1/models")
        (let ((slash (position #\/ chat-url :from-end t :start (or (search "://" chat-url) 0))))
          (if slash (concatenate 'string (subseq chat-url 0 slash) "/v1/models")
              (concatenate 'string chat-url "/v1/models"))))))

(defun llama-model-size (model)
  "A comparable size for a /v1/models entry, or NIL when the server does not
   say.  llama.cpp's router reports the model's own file size in bytes; a
   plain server reports nothing, which is fine — there is only one model
   there.  Falls back to the parameter count some servers expose."
  (let ((size (gethash "size" model))
        (meta (gethash "meta" model)))
    (or (and (numberp size) size)
        (and (hash-table-p meta)
             (or (let ((n (gethash "n_params" meta))) (and (numberp n) n))
                 ;; the router also reports the file size, but nested in meta
                 (let ((n (gethash "size" meta))) (and (numberp n) n)))))))

(defun llama-model-loaded-p (model)
  "True when the server reports MODEL as loaded.  llama.cpp's router says so
   under status.value; a plain server says nothing, which is not \"unloaded\"."
  (let ((st (gethash "status" model)))
    (and (hash-table-p st) (equal (gethash "value" st) "loaded"))))

(defun llama-pick-model (models)
  "Choose from a server's /v1/models list.  One model is the common case and
   is taken as-is.  With several, prefer one the server already has LOADED —
   asking for an unloaded model makes the router load it, which is a
   multi-gigabyte side effect nobody asked for — and among equals take the
   LARGEST, since a bigger model is the better default.  Returns (values ID
   REASON), or (values NIL REASON) when there is nothing to choose from."
  (let ((cands (remove-if-not (lambda (m) (and (hash-table-p m)
                                               (let ((id (gethash "id" m)))
                                                 (and (stringp id) (plusp (length id))))))
                              (coerce models 'list))))
    (cond
      ((null cands) (values nil "the server listed no models"))
      ((null (rest cands)) (values (gethash "id" (first cands)) "the only model it serves"))
      (t (let* ((loaded (remove-if-not #'llama-model-loaded-p cands))
                ;; Prefer a model the server already has in memory, but only
                ;; when it is the ONLY one loaded: with several loaded there is
                ;; no reason to prefer any of them, and the largest is the
                ;; better default.  (The router reports every model's size, so
                ;; "largest" is a real comparison, not a guess.)
                (pool (if (= (length loaded) 1) loaded cands))
                (sized (remove-if-not #'llama-model-size pool))
                (pick (if sized
                          (reduce (lambda (a b)
                                    (if (> (llama-model-size a) (llama-model-size b)) a b))
                                  sized)
                          (first pool))))
           (values (gethash "id" pick)
                   (format nil "~D models; picked the ~:[first~;largest~]~:[~;, already loaded~]"
                           (length cands) (and sized t)
                           (and (eq pool loaded) t))))))))

(defun llama-ask-model (chat-url &key (timeout 5))
  "Ask the llama.cpp server at CHAT-URL which model to use.  Returns (values
   MODEL REASON), or (values NIL REASON) if it could not be determined — the
   caller decides whether that is fatal.  Never signals: a server that does
   not implement /v1/models is not an error, it just leaves the model unset,
   which is exactly right for a single-model server."
  (handler-case
      (let* ((body (http:get (llama-models-url chat-url) :read-timeout timeout))
             (parsed (json:parse body))
             (data (and (hash-table-p parsed) (gethash "data" parsed))))
        (if (or (vectorp data) (listp data))
            (llama-pick-model data)
            (values nil "the server's /v1/models had no model list")))
    (error (e) (values nil (format nil "could not ask the server (~A)" (type-of e))))))

(defun llama-resolve-model (chat-url &key (timeout 5))
  "The model to send to the llama.cpp server at CHAT-URL, asked of the server
   itself.  Returns (values MODEL REASON), MODEL being NIL when the server
   could not be asked — which is not an error: a single-model server wants no
   model field, so NIL is the right answer there.  REASON is a short phrase
   for the operator either way."
  (multiple-value-bind (model reason) (llama-ask-model chat-url :timeout timeout)
    (if model
        (values model reason)
        (values nil (format nil "~A; sending no model field" reason)))))

(defun set-backend-llama! (&key (url *llama-url*) model (ask t))
  "Switch the LLM client back to the local llama.cpp server.  URL defaults to
   *LLAMA-URL* (http://localhost:9931/v1/chat/completions); pass :URL to point
   at a server on another port.

   MODEL is the model id to send.  When it is NIL and ASK is true (the
   default) the server is asked what it serves — a single-model server needs
   no model field at all, but llama.cpp's ROUTER mode 400s a request that
   omits one, so the id has to come from somewhere.  Pass :ASK NIL to skip the
   round-trip and send no model field, and :MODEL to name one outright."
  (setf *llm-backend*    :llama
        *llm-url*        url
        *llm-auth-token* nil
        *llm-model*      (or model (and ask (llama-resolve-model url)))
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
         (*llm-url* *llama-url*)
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
   that is --confidential-tinfoil's job, and it is a property of the session."
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
