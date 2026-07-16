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
;;;   - jzon serializes CL T -> JSON true and CL NIL -> JSON false
;;;     (NOT null). Keep that in mind when building bodies.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (ql:quickload '(:dexador :com.inuoe.jzon) :silent t))

(defpackage #:operandi.llm
  (:use #:cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:export #:*llm-url* #:*llm-default-max-tokens* #:*llm-read-timeout*
           #:*llm-auth-token* #:*llm-model* #:*llm-backend*
           #:llm-chat #:ht #:extract-json
           #:use-openrouter #:use-llama
           #:with-llama #:with-openrouter #:openrouter-credits
           #:*usage-sink* #:usage #:make-usage #:copy-usage
           #:usage-calls #:usage-prompt-tokens #:usage-completion-tokens
           #:usage-cost-usd #:usage-cached-tokens #:with-usage-accounting
           #:fold-usage #:usage-incf #:usage-summary))

(in-package #:operandi.llm)

(defparameter *llm-url* "http://127.0.0.1:8081/v1/chat/completions")
(defparameter *llm-default-max-tokens* 256)
(defparameter *llm-read-timeout* 120)

(defparameter *llm-backend* :llama
  ":LLAMA = local llama.cpp at *llm-url* (default).
   :OPENROUTER = OpenRouter API; *llm-url*, *llm-auth-token*, and
   *llm-model* must be set. Use USE-OPENROUTER to flip.")

(defparameter *llm-auth-token* nil
  "Bearer token for the LLM endpoint, or NIL. Required for openrouter.")

(defparameter *llm-model* nil
  "Model name string. Required for openrouter. Cheapest-first picks:
   minimax/minimax-m2.7 ($0.30/M in), deepseek/deepseek-v4-pro ($0.45),
   moonshotai/kimi-k2.6 ($0.75), anthropic/claude-sonnet-4-5 ($3 — 10x).
   NIL on local llama.cpp.")

(defun ht (&rest pairs)
  "Build a string-keyed hash-table for jzon serialization."
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
   the parsed jzon hash-table, or NIL if no valid object is found."
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
                                        (jzon:parse
                                         (subseq text start (1+ i)))
                                      (error () nil))))))))))))

(defparameter *llm-max-retries* 4
  "Retries for a TRANSIENT LLM HTTP failure (429 rate-limit, 5xx, network/
   timeout) before giving up. Backoff is exponential. A provider 429 must not
   kill a whole scan/report — it usually clears within a few seconds.")

(defun %llm-post (url body headers)
  "POST a chat completion with retry + exponential backoff on transient
   failures. Retries HTTP 429 and 5xx and network/timeout errors; resignals
   non-transient failures (4xx other than 429) immediately."
  (let ((attempt 0))
    (loop
      (handler-case
          (return (dex:post url :content body :headers headers :keep-alive nil
                                :connect-timeout 5 :read-timeout *llm-read-timeout*))
        (dex:http-request-failed (e)
          (let ((code (ignore-errors (dex:response-status e))))
            (if (and (or (eql code 429) (and (integerp code) (>= code 500)))
                     (< attempt *llm-max-retries*))
                (progn (sleep (min 30 (* 3 (expt 2 attempt)))) (incf attempt))
                (error e))))           ; 4xx (not 429) etc. — not recoverable
        (error (e)                      ; network / read-timeout — transient
          (if (< attempt *llm-max-retries*)
              (progn (sleep (min 30 (* 3 (expt 2 attempt)))) (incf attempt))
              (error e)))))))

(defun llm-chat (prompt &key system
                             (max-tokens *llm-default-max-tokens*)
                             (temperature 0.0)
                             think
                             extra)
  "POST a chat completion. Backend-aware: on :LLAMA includes
   chat_template_kwargs.enable_thinking; on :OPENROUTER sends
   model + Authorization. Returns (values content reasoning timings
   raw-parsed)."
  (let* ((msgs (let ((acc '()))
                 (when system
                   (push (ht "role" "system" "content" system) acc))
                 (push (ht "role" "user" "content" prompt) acc)
                 (coerce (nreverse acc) 'vector)))
         (body-ht (ht "messages" msgs
                      "max_tokens" max-tokens
                      "temperature" temperature)))
    ;; Local llama.cpp respects this Qwen kwarg; openrouter would 400.
    (when (eq *llm-backend* :llama)
      (setf (gethash "chat_template_kwargs" body-ht)
            (ht "enable_thinking" (if think t nil))))
    (when *llm-model*
      (setf (gethash "model" body-ht) *llm-model*))
    ;; OpenRouter only reports real USD cost when asked; harmless to llama.
    (when (eq *llm-backend* :openrouter)
      (setf (gethash "usage" body-ht) (ht "include" t)))
    (loop for (k v) on extra by #'cddr
          do (setf (gethash (string-downcase (string k)) body-ht) v))
    (let* ((body (with-output-to-string (s)
                   (jzon:stringify body-ht :stream s)))
           (headers (cond
                      (*llm-auth-token*
                       `(("Content-Type" . "application/json")
                         ("Authorization" . ,(concatenate 'string
                                                          "Bearer "
                                                          *llm-auth-token*))))
                      (t '(("Content-Type" . "application/json")))))
           (resp (%llm-post *llm-url* body headers))
           (parsed (jzon:parse resp))
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
          (let* ((resp (dex:get "https://openrouter.ai/api/v1/credits"
                                :headers `(("Authorization" . ,(concatenate 'string "Bearer " token)))
                                :keep-alive nil :connect-timeout 5 :read-timeout 15))
                 (d (gethash "data" (jzon:parse resp)))
                 (total (gethash "total_credits" d))
                 (usage (gethash "total_usage" d)))
            (values (- total usage) total usage))
        (error () nil)))))

(defun use-openrouter (&key (model "minimax/minimax-m2.7")
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
               "~&use-openrouter: token file ~A missing or empty~%"
               token-file)
       nil)
      (t
       (setf *llm-backend*    :openrouter
             *llm-url*        "https://openrouter.ai/api/v1/chat/completions"
             *llm-auth-token* token
             *llm-model*      model
             *llm-read-timeout* 240)
       (format t "~&[llm] backend=openrouter model=~A~%" model)
       t))))

(defun use-llama (&key (url "http://127.0.0.1:8081/v1/chat/completions"))
  "Switch the LLM client back to the local llama.cpp server."
  (setf *llm-backend*    :llama
        *llm-url*        url
        *llm-auth-token* nil
        *llm-model*      nil
        *llm-read-timeout* 120)
  (format t "~&[llm] backend=llama url=~A~%" url)
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
         (*llm-read-timeout* 120))
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
                (*llm-read-timeout* 240))
            ,@body))))))
