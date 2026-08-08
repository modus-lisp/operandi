;;; src/engine.lisp
;;;
;;; The agent loop. Sends a chat completion request to whatever
;;; backend operandi.llm is currently configured for — local llama.cpp
;;; (port 8081) by default, or OpenRouter via use-openrouter — checks
;;; if the response contains tool_calls, executes them, appends
;;; results to the conversation, and loops until the model returns a
;;; final text answer or we hit MAX-ITERATIONS.
;;;
;;; This is the "Claude Code in Lisp" core — a ReAct-style loop
;;; against the OpenAI-compatible /v1/chat/completions protocol:
;;;
;;;   request:  {messages: [...], tools: [{type:"function", function:...}]}
;;;   response: {message: {role:"assistant",
;;;                        content: ""|"...",
;;;                        tool_calls: [{id, type, function:{name,arguments}}]}}
;;;
;;; We echo back tool results as messages of role:"tool" with the
;;; tool_call_id from the assistant message.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (ql:quickload '(:dexador :com.inuoe.jzon) :silent t))

(defpackage #:operandi.engine
  (:use #:cl)
  (:local-nicknames (#:dex   #:dexador)
                    (#:jzon  #:com.inuoe.jzon)
                    (#:llm   #:operandi.llm)
                    (#:tools #:operandi.tools)
                    (#:hooks #:operandi.hooks)
                    (#:sf    #:operandi.safefetch))
  (:export #:run
           #:preflight-model
           #:*max-iterations*
           #:*max-empty-turns*
           #:*prompt-cache*
           #:*stream*
           #:*on-token*
           #:*subagent-usage*
           #:*subagents*
           #:*context-token-budget*
           #:*do-chat-max-tokens*
           #:*tool-result-keep-chars*
           #:*compact-keep-last*
           #:estimate-tokens #:*token-estimator* #:*chars-per-token*
           #:*offload-dir*
           #:*base-system-prompt*
           #:*default-system-prompt*))

(in-package #:operandi.engine)

(defun %env-int (name default)
  "DEFAULT unless env var NAME parses as an integer — lets a caller (e.g. a swarm
   worker driving a large-context frontier model) raise these limits per-run
   without editing the source: OPERANDI_MAX_ITERS / OPERANDI_CONTEXT_BUDGET."
  (let ((v (uiop:getenv name)))
    (or (and v (ignore-errors (parse-integer v :junk-allowed t))) default)))

(defparameter *max-iterations* (%env-int "OPERANDI_MAX_ITERS" 80)
  "Hard cap on the agentic loop. Frontier models doing real analysis
   (e.g. computing correlations across thousands of rows) commonly
   want 30+ tool calls; with auto-compaction keeping context bounded
   the cost is manageable. The model returning text without a tool
   call exits sooner.  Override via OPERANDI_MAX_ITERS.")

(defparameter *chat-retries* 2
  "Number of retries on apparent transient errors (HTTP 5xx, empty
   choices array, OpenRouter 200-with-error-body). Sleep doubles
   each time starting at *CHAT-RETRY-SLEEP*.")

(defparameter *chat-retry-sleep* 0.5
  "Initial sleep before retry, in seconds. Doubles per retry.")

(defparameter *context-token-budget* (%env-int "OPERANDI_CONTEXT_BUDGET" 24000)
  "Primary compaction trigger: when the running conversation's estimated
   tokens exceed this, compact. Message count is a poor proxy — a single
   50KB tool result is ~12k tokens, so a few big outputs blow the model's
   window long before any message-count trigger. Set to roughly half the
   model's context so there's headroom for the reply.  The 24k default suits
   a small LOCAL model; a large-context frontier worker should raise it
   (OPERANDI_CONTEXT_BUDGET) — a too-small budget makes an agent THRASH:
   compaction evicts earlier findings faster than it can act on them.")

(defparameter *compact-keep-last* 14
  "Number of recent messages preserved verbatim during compaction.")

(defparameter *tool-result-keep-chars* 1200
  "Tier-1 compaction (before any LLM summary) truncates each large OLD
   tool result to this many leading chars + a note. Big tool outputs are
   the bulk of the tokens, so trimming them reclaims most space cheaply
   and keeps the conversation's structure intact.")

(defparameter *compact-after-messages* 60
  "DEPRECATED — compaction is token-driven now (see *CONTEXT-TOKEN-BUDGET*).
   Retained so any host that set it doesn't break; no longer consulted.")

(defvar *chars-per-token* 4.0d0
  "Running characters-per-token estimate for the ACTIVE model, used by the
   default token estimator. Starts at the classic 4 and is calibrated from
   the prompt_tokens the API reports vs the chars we sent — so it self-
   corrects to whatever tokenizer the model actually uses, without needing
   that tokenizer. (Tools/system overhead the API counts but we don't makes
   it run slightly conservative — compacts a touch early, the safe way.)")

(defun calibrate-chars-per-token (sent-chars actual-tokens)
  "Fold an observed (chars, tokens) pair into *CHARS-PER-TOKEN* via an EMA,
   clamped so one odd response can't wreck the estimate."
  (when (and (integerp sent-chars) (plusp sent-chars)
             (integerp actual-tokens) (plusp actual-tokens))
    (let ((observed (/ (float sent-chars 1d0) actual-tokens))
          (alpha 0.2d0))
      (setf *chars-per-token*
            (max 1.5d0 (min 10d0
                            (+ (* (- 1d0 alpha) *chars-per-token*)
                               (* alpha observed))))))))

(defun msg-chars (m)
  "Character weight of one message: content + tool_call arguments + a small
   per-message overhead."
  (let ((chars 4))
    (let ((c (gethash "content" m)))
      (when (stringp c) (incf chars (length c))))
    (let ((tcs (gethash "tool_calls" m)))
      (when (and tcs (or (vectorp tcs) (listp tcs)))
        (map nil (lambda (tc)
                   (let* ((fn (and (hash-table-p tc) (gethash "function" tc)))
                          (a (and fn (gethash "arguments" fn))))
                     (when (stringp a) (incf chars (length a)))))
             (if (listp tcs) (coerce tcs 'vector) tcs))))
    chars))

(defun estimate-tokens-chars (messages)
  "Default estimator: total chars / the (calibrated) chars-per-token."
  (ceiling (reduce #'+ messages :key #'msg-chars :initial-value 0)
           *chars-per-token*))

(defvar *token-estimator* 'estimate-tokens-chars
  "Function (messages) -> integer, behind ESTIMATE-TOKENS. Default is the
   calibrating chars-based estimator; a deployment can set this to a real
   tokenizer-backed function for its model.")

(defun estimate-tokens (messages)
  "Estimated token count for a message list, via *TOKEN-ESTIMATOR*."
  (funcall (if (functionp *token-estimator*)
               *token-estimator*
               (symbol-function *token-estimator*))
           messages))

(defparameter *compaction-prompt*
  "You are the compaction summarizer for an autonomous agent. Read
the conversation snippet below — a sequence of assistant messages,
tool calls, and tool results — and produce a TERSE summary in 100-200
words covering: (1) what the agent learned or established as fact,
(2) what tool calls succeeded or failed and why, (3) any persistent
state changes (files written, notes recorded). DO NOT recap the
original task or the agent's reasoning style. Pure facts. The output
will replace the snippet in the agent's running conversation, so the
agent should be able to continue with just the summary as context.")

(defun summarize-turns (turns)
  "Call llm-chat to compress a list of message hash-tables into a
   single string. Returns the summary as a string, or NIL on error."
  (let* ((rendered (with-output-to-string (s)
                     (dolist (m turns)
                       (let* ((role (gethash "role" m))
                              (raw  (gethash "content" m))
                              ;; Content can be NIL on assistant turns
                              ;; that issued only tool_calls (no text).
                              ;; Coerce to string before LENGTH.
                              (content (cond
                                         ((null raw) "")
                                         ((stringp raw) raw)
                                         (t (princ-to-string raw)))))
                         (format s "[~A] ~A~%" role
                                 ;; Tool results are already bounded by the Read
                                 ;; budget; keep enough that the summarizer sees the
                                 ;; substance instead of a stub of each turn.
                                 (if (> (length content) 16000)
                                     (concatenate 'string
                                                  (subseq content 0 16000)
                                                  "...[truncated]")
                                     content)))))))
    (handler-case
        (multiple-value-bind (text)
            (llm:llm-chat rendered :system *compaction-prompt*
                                   :max-tokens 512
                                   :temperature 0.0
                                   :think nil)
          text)
      (error () nil))))

(defun safe-tail-start (messages keep-last)
  "Compute the index where the preserved tail should begin. Snaps
   backward if the candidate index would split a tool-call turn —
   tool-result messages must follow their parent assistant message
   per the OpenAI protocol, so the tail must begin at a non-tool role."
  (let* ((n (length messages))
         (idx (max 2 (- n keep-last))))
    (loop while (and (< idx n)
                     (let ((m (nth idx messages)))
                       (string= (gethash "role" m) "tool")))
          do (decf idx))
    idx))

(defun render-pinned-todos ()
  "Render the live TODO list as a checklist so it survives compaction verbatim.
   The plan is exactly what the agent must not lose when the middle is summarized —
   without this, an agent re-discovers (and re-reads) what it already established.
   Returns NIL when there are no todos."
  (let ((todos operandi.tools:*todos*))
    (when todos
      (with-output-to-string (s)
        (format s "## Current plan (pinned across compaction — do not re-derive)~%")
        (dolist (td todos)
          (format s "  ~A ~A: ~A~%"
                  (cond ((string= (getf td :status) "completed") "[x]")
                        ((string= (getf td :status) "in_progress") "[~]")
                        (t "[ ]"))
                  (getf td :id) (getf td :subject)))))))

(defun trim-old-tool-results (messages keep-last)
  "Tier-1 compaction: truncate each large tool result in the MIDDLE (older
   than the last KEEP-LAST messages) to *TOOL-RESULT-KEEP-CHARS* + a note,
   leaving the head (system + first user) and the recent tail intact.
   Cheap, no LLM, preserves structure. Returns a fresh list."
  (let ((tail-start (safe-tail-start messages keep-last)))
    (loop for m in messages
          for i from 0
          for c = (gethash "content" m)
          collect (if (and (>= i 2) (< i tail-start)
                           (string= (gethash "role" m) "tool")
                           (stringp c) (> (length c) *tool-result-keep-chars*))
                      (ht "role" "tool"
                          "tool_call_id" (gethash "tool_call_id" m)
                          "content"
                          (format nil "~A~%…[~:D chars trimmed to save context]"
                                  (subseq c 0 *tool-result-keep-chars*)
                                  (- (length c) *tool-result-keep-chars*)))
                      m))))

(defparameter *offload-dir*
  (namestring (merge-pathnames ".operandi/offload/" (user-homedir-pathname)))
  "Where compaction OFFLOADS the raw displaced middle. Compaction is then
   reversible — nothing is destroyed; the agent can Read the file to recover
   any detail the summary dropped (Fable's point: lossy compaction is an
   irreversibility mistake; offloading saves the same tokens, reversibly).")
(defvar *offload-counter* 0)

(defun render-turns (turns)
  "Render TURNS (message hash-tables) to plain text, FULL content — this is
   the raw the agent gets back on recall, so no truncation."
  (with-output-to-string (s)
    (dolist (m turns)
      (let ((role (gethash "role" m)) (c (gethash "content" m))
            (tcs (gethash "tool_calls" m)))
        (format s "[~A] ~A~%" role
                (cond ((stringp c) c) ((null c) "") (t (princ-to-string c))))
        (when (and tcs (or (vectorp tcs) (listp tcs)))
          (map nil (lambda (tc)
                     (let ((fn (and (hash-table-p tc) (gethash "function" tc))))
                       (when (hash-table-p fn)
                         (format s "    -> ~A(~A)~%"
                                 (gethash "name" fn) (gethash "arguments" fn)))))
               (if (listp tcs) (coerce tcs 'vector) tcs)))))))

(defun offload-write (turns)
  "Write TURNS to a fresh file under *OFFLOAD-DIR*; return its path, or NIL."
  (handler-case
      (progn
        (ensure-directories-exist *offload-dir*)
        (let ((path (merge-pathnames (format nil "turns-~D.txt" (incf *offload-counter*))
                                     *offload-dir*)))
          (with-open-file (s path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create)
            (write-string (render-turns turns) s))
          (namestring path)))
    (error () nil)))

(defun offload-middle (messages)
  "Tier-2 compaction, REVERSIBLE: write the displaced middle to a file (so
   nothing is destroyed — the agent can Read it back) and replace it with a
   pointer plus an optional summary. Preserves system + first user + the
   last *COMPACT-KEEP-LAST* messages. Returns MESSAGES unchanged only if
   there is no middle to offload."
  (cond
    ((<= (length messages) (+ 2 *compact-keep-last*))
     messages)
    (t
     (let* ((tail-start (safe-tail-start messages *compact-keep-last*))
            (head (subseq messages 0 2))   ; system + first user
            (middle (subseq messages 2 tail-start))
            (tail (subseq messages tail-start)))
       (if (null middle)
           messages
           (let ((path (offload-write middle))
                 (summary (summarize-turns middle)))
             (append head
                     (list (ht "role" "assistant"
                               "content"
                               (format nil "[~D earlier turns compacted to save context~@[; OFFLOADED (not lost) to ~A — Read that file to recover detail~].~@[~%~%~A~]~@[~%~%Summary:~%~A~]"
                                       (length middle) path
                                       (render-pinned-todos) summary)))
                     tail)))))))

(defun compact-messages (messages)
  "Bring MESSAGES under *CONTEXT-TOKEN-BUDGET*. Tier 1: trim old large
   tool results (cheap, no LLM, structure-preserving). Tier 2, only if
   still over budget: OFFLOAD the middle (reversibly — raw kept in a file)
   with a summary pointer. Returns a list at or below budget where
   possible; never grows the input."
  (if (<= (estimate-tokens messages) *context-token-budget*)
      messages
      (let ((trimmed (trim-old-tool-results messages *compact-keep-last*)))
        (if (<= (estimate-tokens trimmed) *context-token-budget*)
            trimmed
            (offload-middle trimmed)))))

(defun maybe-compact (messages verbose)
  "Compact MESSAGES if it's over the token budget; else return it as-is."
  (if (<= (estimate-tokens messages) *context-token-budget*)
      messages
      (let ((out (compact-messages messages)))
        (when verbose
          (format t "~&[operandi] compacted ~D->~D tok (~D->~D msgs)~%"
                  (estimate-tokens messages) (estimate-tokens out)
                  (length messages) (length out)))
        out)))

(defparameter *base-system-prompt*
  "You are operandi, an autonomous agent running inside the operator's
sovereign Lisp environment. The model behind you may be a small local
LLM or a frontier model via OpenRouter — you don't need to know which.
You have tools for reading and writing files, running shell commands,
evaluating Lisp in the host SBCL image, and searching code. You also
have a persistent notes file (Remember tool) that travels across runs.

Style:
  * Be concise. Don't explain what you're about to do; just do it.
  * Prefer action over discussion. If a question is unclear, make a
    reasonable assumption and proceed; flag the assumption briefly.
  * When you've completed the task, give a short final answer (one
    short paragraph at most) describing what you did or found.
  * If the task is genuinely impossible or unsafe, say so plainly.

Working discipline (this is how you avoid thrashing):
  * PLAN first. For any multi-step task, write a short TodoWrite plan naming
    the SPECIFIC files and functions you will change and the approach. Your
    todos are pinned across compaction — they are your durable memory, so
    record findings there (a file:line, the exact edit you intend), and
    update them as you go. Don't investigate past what the plan needs.
  * READ narrowly. Locate code with Grep, then Read a REGION with offset/limit.
    Do not read a large file whole — Read bounds its own output and tells you
    the size of the middle it elided; re-Read just that range if you need it.
  * Don't repeat work. Never re-run a search you've already run or re-read a
    file you've already seen — consult your todos/notes instead. If you catch
    yourself re-reading, you've lost the plan; rebuild it from your todos.
  * VERIFY as you go, and END ON GREEN. After any edit that changes code
    structure, run the check/oracle before stacking another edit — don't batch
    unverified edits (a paren slip in one hides the next). Your LAST action before
    stopping must be a passing check (or, if you couldn't reach passing, an honest
    report of the best state you reached and what's still red). Never stop right
    after an edit you haven't re-run — a broken build you didn't look at is worse
    than an unfinished task you described.

Use Eval over Bash for anything involving Lisp code or the host
image's data — every package loaded in the running SBCL image is
callable directly from Eval (the host application decides which
domain packages those are). Bash is for shell idioms; Eval is for
everything else.

When you discover something non-obvious — a schema quirk, a bug in a
helper, a non-trivial fact about the data, a path to a config file —
USE the Remember tool. Notes appear at the top of your next run's
system prompt; future-you will thank you.

You are NOT making predictions, NOT estimating probabilities, NOT
making investment decisions. You're an engineer doing concrete work.

Use tools by calling them. Each tool call's result is appended to
the conversation; the next message you produce should either call
another tool or give a final answer.")

(defun build-system-prompt ()
  "Combine the base prompt with current notes file contents."
  (let ((notes (operandi.tools:load-notes)))
    (cond
      ((zerop (length notes)) *base-system-prompt*)
      (t (format nil "~A~%~%## Persistent notes (from previous runs)~%~%~A"
                 *base-system-prompt* notes)))))

(defparameter *default-system-prompt* nil
  "DEPRECATED — use BUILD-SYSTEM-PROMPT instead so notes load fresh
   each run. Kept as a back-compat handle.")

(defun ht (&rest pairs) (apply #'llm:ht pairs))

(defparameter *do-chat-max-tokens* (%env-int "OPERANDI_MAX_TOKENS" 16384)
  "Default max-tokens for each chat call inside the agent loop. Override
   via OPERANDI_MAX_TOKENS, DEFPARAMETER, or LET.

   This is a CAP, not an allocation — you pay only for what's emitted — so
   it should be generous. It used to be 4096, and that silently broke every
   attempt to write a real source file: the tool_call arguments carrying the
   file body ran past the cap, the provider returned a TRUNCATED arguments
   string (while still reporting finish_reason \"tool_calls\"), the JSON
   failed to parse, and the write no-opped. Reasoning models are worse
   still — the thinking trace is spent from the same budget before a single
   argument byte is emitted.")

(defparameter *do-chat-disable-reasoning* nil
  "If T (and backend is OpenRouter), send reasoning.enabled=false in the
   request body. Set for models that can't compose reasoning_content with
   the tool_calls protocol — chiefly Qwen-thinking variants. Models that
   require reasoning (Minimax M2/M2.5) will 400 if this is enabled.")

(defparameter *max-tool-calls* nil
  "Optional hard cap on total tool calls within a single RUN invocation,
   counting across all iterations. When exceeded, the next tool call is
   short-circuited with a synthetic 'tool budget exhausted' result so the
   model is forced to finalize on the following turn. NIL = no cap.
   Set to e.g. 5 in research-heavy calls where some models (Minimax M2)
   will otherwise search 15+ times.")

(defparameter *prompt-cache* t
  "When T and the model is an Anthropic (Claude) model — which, unlike
   OpenAI/DeepSeek/most OpenRouter providers, does NOT cache prompt
   prefixes automatically — mark the large, stable system message with a
   cache_control breakpoint so its tokens are cached across a run's turns.
   Gated on the model name (a marker could 400 a provider that doesn't
   expect it; those already auto-cache anyway — measured ~74% hit).
   CAVEAT: the request SHAPE is unit-tested (inspect/caching-test.lisp);
   live cache-hit behaviour is UNVERIFIED — the current OpenRouter key has
   no Claude access to test against.")

(defun anthropic-model-p (model)
  (and (stringp model)
       (or (search "claude" model :test #'char-equal)
           (search "anthropic" model :test #'char-equal))))

(defun cache-control-system (messages)
  "Non-destructively return MESSAGES (a vector) with the leading system
   message carrying an Anthropic ephemeral cache_control breakpoint — its
   string content becomes a one-element text content-block array. If the
   first message isn't a plain-text system message, returns MESSAGES as-is."
  (let ((v (coerce messages 'vector)))
    (if (and (plusp (length v))
             (let ((m (aref v 0)))
               (and (hash-table-p m)
                    (equal (gethash "role" m) "system")
                    (stringp (gethash "content" m)))))
        (let ((out (copy-seq v)))
          (setf (aref out 0)
                (ht "role" "system"
                    "content" (vector (ht "type" "text"
                                          "text" (gethash "content" (aref v 0))
                                          "cache_control" (ht "type" "ephemeral")))))
          out)
        v)))

(defparameter *stream* t
  "Stream chat completions (SSE) instead of one blocking response. Gives
   time-to-first-token and live output, and makes a turn interruptible
   mid-generation (Ctrl-C aborts the read). Same request otherwise; the
   assembled result is identical in shape to the blocking path, so the
   rest of the loop is unchanged. Set NIL to force blocking.")

(defvar *on-token* nil
  "When bound to a function of one string arg, streamed CONTENT tokens are
   passed to it as they arrive (the run loop binds it to a live printer
   when verbose). NIL = accumulate silently.")

(defun chat-headers ()
  (if llm:*llm-auth-token*
      `(("Content-Type" . "application/json")
        ("Authorization" . ,(concatenate 'string "Bearer " llm:*llm-auth-token*)))
      '(("Content-Type" . "application/json"))))

(defun build-chat-body (messages tools-vec max-tokens temperature stream)
  "The request body shared by the blocking and streaming paths."
  (let* ((msgs (if (and *prompt-cache* (eq llm:*llm-backend* :openrouter)
                        (anthropic-model-p llm:*llm-model*))
                   (cache-control-system messages)
                   (coerce messages 'vector)))
         (body (ht "messages" msgs "tools" tools-vec "tool_choice" "auto"
                   "max_tokens" max-tokens "temperature" temperature)))
    (when stream
      (setf (gethash "stream" body) t)
      ;; ask providers to send a final usage chunk in the stream
      (setf (gethash "stream_options" body) (ht "include_usage" t)))
    (when (eq llm:*llm-backend* :llama)              ; Qwen-only; openrouter 400s
      (setf (gethash "chat_template_kwargs" body) (ht "enable_thinking" nil)))
    (when (eq llm:*llm-backend* :openrouter)         ; cost/token accounting
      (setf (gethash "usage" body) (ht "include" t)))
    (when (and (eq llm:*llm-backend* :openrouter) *do-chat-disable-reasoning*)
      (setf (gethash "reasoning" body) (ht "enabled" nil)))
    (when llm:*llm-model* (setf (gethash "model" body) llm:*llm-model*))
    body))

(defun do-chat-blocking (messages tools-vec &key (max-tokens *do-chat-max-tokens*)
                                                 (temperature 0.0))
  "One blocking chat completion. Returns the parsed top-level hash table."
  (let ((resp (dex:post llm:*llm-url*
                        :content (with-output-to-string (s)
                                   (jzon:stringify (build-chat-body messages tools-vec
                                                                    max-tokens temperature nil)
                                                   :stream s))
                        :headers (chat-headers) :keep-alive nil
                        :connect-timeout 5 :read-timeout llm:*llm-read-timeout*)))
    (jzon:parse resp)))

(defun parse-sse-line (line)
  "A single SSE line -> the parsed JSON hash for a 'data:' event, :done for
   'data: [DONE]', or NIL (blank / comment / keep-alive / unparseable).
   Tolerates 'data:' with or without the space."
  (when (and (>= (length line) 5) (string= (subseq line 0 5) "data:"))
    (let ((payload (string-trim '(#\Return #\Space) (subseq line 5))))
      (cond ((zerop (length payload)) nil)
            ((string= payload "[DONE]") :done)
            (t (handler-case (jzon:parse payload) (error () nil)))))))

;; Streaming assembly, factored out of the socket read so it's unit-
;; testable: fold each parsed SSE event into a state, then finalize into
;; the blocking-path response shape.
(defstruct sse-state
  (content (make-string-output-stream))
  (tcs (make-hash-table))                ; index -> (list id name args-stream)
  finish usage error)

(defun sse-fold (state evt)
  "Fold one parsed SSE event hash into STATE; fire *ON-TOKEN* for content
   tokens as they arrive."
  ;; OpenRouter can stream a provider error as a 200 SSE event carrying an
  ;; {\"error\":{...}} object (e.g. an exhausted grant / 402 in the body).
  ;; Capture it so sse-finalize surfaces the same 200-with-error-body shape the
  ;; blocking path produces — otherwise the turn silently finalizes blank while
  ;; a usage event still bills a few cents.
  (let ((err (gethash "error" evt)))
    (when err (setf (sse-state-error state) err)))
  (let ((u (gethash "usage" evt)))
    (when (hash-table-p u) (setf (sse-state-usage state) u)))
  (let* ((ch (gethash "choices" evt))
         (choice (and (vectorp ch) (plusp (length ch)) (aref ch 0)))
         (delta (and (hash-table-p choice) (gethash "delta" choice)))
         (fr (and (hash-table-p choice) (gethash "finish_reason" choice))))
    (when (stringp fr) (setf (sse-state-finish state) fr))
    (when (hash-table-p delta)
      (let ((c (gethash "content" delta)))
        (when (and (stringp c) (plusp (length c)))
          (write-string c (sse-state-content state))
          (when *on-token* (funcall *on-token* c))))
      (let ((dt (gethash "tool_calls" delta)))
        (when (vectorp dt)
          (loop for tc across dt do
            (let* ((idx (gethash "index" tc))
                   (e (or (gethash idx (sse-state-tcs state))
                          (setf (gethash idx (sse-state-tcs state))
                                (list nil nil (make-string-output-stream)))))
                   (id (gethash "id" tc)) (fn (gethash "function" tc)))
              (when id (setf (first e) id))
              (when (hash-table-p fn)
                (let ((nm (gethash "name" fn)) (ar (gethash "arguments" fn)))
                  (when nm (setf (second e) nm))
                  (when (stringp ar) (write-string ar (third e))))))))))
    state))

(defun sse-finalize (state)
  "Turn accumulated STATE into a parsed response identical in shape to the
   blocking path: {choices:[{message, finish_reason}], usage?}. If the stream
   carried a provider error and produced no content or tool calls, finalize it
   as a 200-with-error-body ({error, usage?}) — NO choices — so it flows through
   response-has-error-body-p / do-chat-with-retries instead of masquerading as a
   blank but 'successful' turn."
  (let* ((content (get-output-stream-string (sse-state-content state)))
         (tcs (sse-state-tcs state))
         (has-tcs (plusp (hash-table-count tcs))))
    ;; Provider error with nothing usable → surface the error body.
    (when (and (sse-state-error state) (not has-tcs) (zerop (length content)))
      (let ((parsed (ht "error" (sse-state-error state))))
        (when (sse-state-usage state) (setf (gethash "usage" parsed) (sse-state-usage state)))
        (return-from sse-finalize parsed)))
    (let ((msg (ht "role" "assistant" "content" content)))
      (when has-tcs
        (setf (gethash "tool_calls" msg)
              (coerce (loop for i in (sort (loop for k being the hash-keys of tcs collect k) #'<)
                            for e = (gethash i tcs)
                            collect (ht "id" (or (first e) (format nil "call_~A" i))
                                        "type" "function"
                                        "function" (ht "name" (or (second e) "")
                                                       "arguments" (get-output-stream-string (third e)))))
                      'vector)))
      (let ((parsed (ht "choices" (vector (ht "message" msg
                                              "finish_reason" (sse-state-finish state))))))
        (when (sse-state-usage state)
          (setf (gethash "usage" parsed) (sse-state-usage state)))
        parsed))))

(defun do-chat-stream (messages tools-vec &key (max-tokens *do-chat-max-tokens*)
                                               (temperature 0.0))
  "Streaming chat completion. Reads the SSE deltas, feeds CONTENT tokens to
   *ON-TOKEN* as they arrive, and returns the assembled response in the SAME
   shape as the blocking path. Closes the stream on any exit — including a
   Ctrl-C mid-generation."
  (let ((stream (dex:post llm:*llm-url*
                          :content (with-output-to-string (s)
                                     (jzon:stringify (build-chat-body messages tools-vec
                                                                      max-tokens temperature t)
                                                     :stream s))
                          :headers (chat-headers) :keep-alive nil
                          :connect-timeout 5 :read-timeout llm:*llm-read-timeout*
                          :want-stream t))
        (state (make-sse-state)))
    (unwind-protect
         (loop for line = (read-line stream nil :eof)
               until (eq line :eof)
               for evt = (parse-sse-line line)
               until (eq evt :done)
               when (hash-table-p evt) do (sse-fold state evt))
      (ignore-errors (close stream)))
    (sse-finalize state)))

(defun do-chat (messages tools-vec &rest keys)
  "Single chat-completion call, streaming or blocking per *STREAM*. Returns
   the parsed top-level hash table either way. Backend-aware via the
   operandi.llm specials."
  (if *stream*
      (apply #'do-chat-stream messages tools-vec keys)
      (apply #'do-chat-blocking messages tools-vec keys)))

(defun extract-message (parsed)
  (when (hash-table-p parsed)
    (let ((choices (gethash "choices" parsed)))
      (and (vectorp choices) (plusp (length choices))
           (gethash "message" (aref choices 0))))))

(defun response-finish-reason (parsed)
  "The first choice's finish_reason, or NIL. \"length\" means the model was
   cut off at the output cap — for a reasoning model that can happen before
   it emits any content or tool call at all, which otherwise reads as a
   mysterious empty turn."
  (when (hash-table-p parsed)
    (let ((choices (gethash "choices" parsed)))
      (and (vectorp choices) (plusp (length choices))
           (gethash "finish_reason" (aref choices 0))))))

(defun response-has-error-body-p (parsed)
  "OpenRouter sometimes returns 200 with a body like
   {\"error\":{\"message\":...,\"code\":N}}. Detect that so we can retry."
  (and parsed (hash-table-p parsed)
       (gethash "error" parsed)
       (not (gethash "choices" parsed))))

(defun provider-error-text (parsed)
  "Human-readable text of a 200-with-error-body, e.g.
   \"[provider error] Insufficient credits (402)\" — so the user sees WHY a turn
   came back empty instead of a bare blank. NIL if PARSED has no error object."
  (let ((err (and (hash-table-p parsed) (gethash "error" parsed))))
    (when err
      (let* ((msg  (and (hash-table-p err) (gethash "message" err)))
             (code (and (hash-table-p err) (gethash "code" err))))
        (format nil "[provider error] ~A~@[ (~A)~]"
                (if (stringp msg) msg (princ-to-string err))
                (and code (princ-to-string code)))))))

(defun http-error->parsed (e)
  "Turn a dex:http-request-failed into an error-body parsed ({error:{message,code}}),
   preferring the provider's own JSON error message from the response body. So a
   404/402/401 surfaces its real reason via provider-error-text rather than a bare
   '[empty response from model]'. Non-retryable 4xx are marked so the loop stops."
  (let* ((code (ignore-errors (dex:response-status e)))
         (raw (ignore-errors (dex:response-body e)))
         (body (cond ((stringp raw) raw)
                     ((typep raw '(vector (unsigned-byte 8)))
                      (ignore-errors (sb-ext:octets-to-string raw :external-format :utf-8)))
                     ;; streaming request (:want-stream t) → body is a stream; slurp it.
                     ((and raw (streamp raw))
                      (ignore-errors
                        (with-output-to-string (o)
                          (loop for line = (read-line raw nil nil) while line
                                do (write-line line o)))))
                     (t nil)))
         (parsed (and body (ignore-errors (jzon:parse body))))
         (inner (and (hash-table-p parsed) (gethash "error" parsed)))
         (msg (cond ((and (hash-table-p inner) (stringp (gethash "message" inner)))
                     (gethash "message" inner))
                    ((and (stringp body) (plusp (length body))) (subseq body 0 (min 300 (length body))))
                    (t (format nil "HTTP ~A" code)))))
    (llm:ht "error" (llm:ht "message" msg "code" (or code 0)))))

(defun preflight-model (&key (timeout 12))
  "Startup preflight: confirm the CURRENT backend/model/token can actually serve a
   request, so a bad model slug / exhausted grant / provider-allowlist miss / a
   local server that isn't up fails LOUD at launch instead of blank-per-turn.
   Sends one 1-token, tool-less ping and reads the provider's error reason.
   Returns (values OK-P REASON). Never signals."
  (handler-case
      (let* ((msgs (list (ht "role" "user" "content" "ping")))
             (parsed (handler-case
                         (let ((*stream* nil))
                           (do-chat-blocking msgs #() :max-tokens 1))
                       (dex:http-request-failed (e) (http-error->parsed e)))))
        (cond
          ((response-has-error-body-p parsed)
           (values nil (or (provider-error-text parsed) "provider returned an error")))
          ;; A 200 (even empty content) means the model is accepted + usable.
          ((and (hash-table-p parsed) (gethash "choices" parsed)) (values t nil))
          (t (values t nil))))              ; couldn't tell → don't block launch
    (error (e)
      (values nil (format nil "cannot reach ~A (~A)" llm:*llm-url* (type-of e))))))

(defun do-chat-with-retries (messages tools-vec &key (verbose nil))
  "Wrap do-chat with up to *CHAT-RETRIES* retries on apparent transient
   failures: HTTP errors, OpenRouter 200-with-error-body, or responses
   missing a usable message. Returns parsed body."
  (let ((attempt 0)
        (sleep *chat-retry-sleep*))
    (loop
      (let* ((parsed (handler-case (do-chat messages tools-vec)
                       (dex:http-request-failed (e)
                         ;; A real HTTP 4xx/5xx (e.g. OpenRouter 404 "No allowed
                         ;; providers", 402 grant, 401 bad key). dex discards the
                         ;; body into the condition — recover it as an error-body
                         ;; parsed so provider-error-text surfaces the REAL reason
                         ;; instead of a mysterious "[empty response from model]".
                         (when verbose
                           (format t "~&[operandi] http ~A (try ~A)~%"
                                   (ignore-errors (dex:response-status e)) (1+ attempt)))
                         (http-error->parsed e))
                       (error (e)
                         (when verbose
                           (format t "~&[operandi] http err (try ~A): ~A~%"
                                   (1+ attempt) e))
                         nil)))
             (err (and parsed (response-has-error-body-p parsed)))
             (msg (and parsed (extract-message parsed))))
        (cond
          ;; Success path: we got a usable message.
          ((and parsed msg) (return parsed))
          ;; Out of retries.
          ((>= attempt *chat-retries*)
           (when verbose
             (format t "~&[operandi] giving up after ~A tries~%" (1+ attempt)))
           (return parsed))
          ;; Otherwise: retry.
          (t
           (when verbose
             (format t "~&[operandi] empty/error response (try ~A); retrying in ~,1Fs~A~%"
                     (1+ attempt) sleep
                     (if err " (provider error in body)" "")))
           (sleep sleep)
           (setf sleep (* 2 sleep))
           (incf attempt)))))))

(defun msg-text (msg)
  "The assistant message's textual content as a NON-EMPTY string, or NIL.
   The chat API returns JSON null for a toolless empty turn, which jzon
   parses to the symbol NULL (not a string, not NIL) — normalize that,
   plus blank/whitespace-only strings, to a single NIL 'no text'
   sentinel so callers never leak \"NULL\" or \"\" as a final answer."
  (let ((c (and msg (gethash "content" msg))))
    (when (and (stringp c)
               (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) c))))
      c)))

(defun message-tool-calls (msg)
  "Return list of tool-call hash tables, or NIL."
  (let ((tcs (and msg (gethash "tool_calls" msg))))
    (when (vectorp tcs) (coerce tcs 'list))))

(defun parse-tool-args (tc)
  "Parse a tool call's arguments. Returns (values args error-string).

   A non-nil second value means the arguments did NOT parse — nearly always
   because the model ran out of output budget mid-argument and the provider
   handed back a truncated string (see *DO-CHAT-MAX-TOKENS*). Report that to
   the model instead of invoking the tool with empty args: a silent no-op
   looks like success to the agent, which then reasons on top of a write
   that never happened."
  (let* ((fn (gethash "function" tc))
         (raw (and fn (gethash "arguments" fn))))
    (handler-case (values (and raw (jzon:parse raw)) nil)
      (error (e)
        (values (make-hash-table :test 'equal)
                (format nil "~A chars, ~A" (length (or raw "")) e))))))

(defun tool-call-name (tc)
  (let ((fn (gethash "function" tc)))
    (and fn (gethash "name" fn))))

(defun assistant-msg-from-response (msg)
  "Build the assistant message to append to the running conversation.
   Preserve the tool_calls array verbatim — the OpenAI protocol
   requires the assistant turn that issued the tool calls to be
   present before any tool-result turns."
  (let ((h (ht "role" "assistant"
                "content" (or (msg-text msg) ""))))
    (let ((tcs (gethash "tool_calls" msg)))
      (when (and tcs (or (listp tcs) (vectorp tcs)))
        (setf (gethash "tool_calls" h) tcs)))
    h))

(defun tool-result-msg (tool-call-id content)
  (ht "role" "tool"
      "tool_call_id" tool-call-id
      "content" (or content "")))

(defparameter *max-empty-turns* 2
  "How many times to nudge a model that ends a turn with empty content
   AND no tool call (a stall — often a null-content final message) before
   giving up and salvaging. Each nudge costs one iteration.")

(defvar *subagent-usage* nil
  "Per-run accumulator (a LLM:USAGE struct) that RUN binds fresh. The
   Task/Fan tools add each subagent's usage into it, and RUN folds it into
   the total it returns — so a run's reported cost/tokens include all of
   its (transitively nested) subagents. Because tool calls run in the
   parent's thread, the parent's binding is what Task/Fan see; Fan sums
   its worker-thread results and adds them here from the parent thread.")

(defvar *subagents* nil
  "Per-run registry of persistent, resumable subagents: handle -> a
   subagent record (see the Spawn/SendMessage tools). RUN binds it fresh,
   so it's scoped to one run (single-threaded — tool calls run in sequence
   — hence no lock) and discarded when the run ends (no cross-run leak).")

(defun last-substantive (messages)
  "Best salvage when the model ends with only empty turns: the most
   recent non-empty assistant text, else the most recent tool result."
  (or (loop for m in (reverse messages)
            when (and (string= (gethash "role" m) "assistant") (msg-text m))
              return (msg-text m))
      (loop for m in (reverse messages)
            when (string= (gethash "role" m) "tool")
              return (let ((c (gethash "content" m)))
                       (when (and (stringp c) (plusp (length c)))
                         (format nil "[no final summary from the model; last tool result follows]~%~A" c))))))

(defun run (initial-prompt &key
                             (system nil)
                             (tool-names (tools:default-tools))
                             (max-iterations *max-iterations*)
                             (verbose t)
                             (history nil))
  "Run the agent loop. INITIAL-PROMPT is the user's task description.
   TOOL-NAMES is a list of registered tool names to expose. Returns
   (values final-text full-message-history n-iterations)."
  (let* ((run-id (format nil "~A-~A"
                          (- (get-universal-time) 2208988800)
                          (random 100000)))
         (hooks:*current-run-id* run-id)
         (tools:*file-read-state* (make-hash-table :test 'equal))
         (tools:*todos* nil)
         (tools-vec (tools:tools-as-openai-array tool-names))
         (sys (or system (build-system-prompt)))
         (messages (or history
                       (list (ht "role" "system" "content" sys)
                             (ht "role" "user"   "content" initial-prompt))))
         (n 0)
         (tool-call-count 0)
         (empty-turns 0)
         (usage (llm:make-usage))
         (*subagent-usage* (llm:make-usage))
         (*subagents* (make-hash-table :test 'equal))
         (sf:*fetch-history* (make-hash-table :test 'equal))
         (sf:*fetch-raw-cache* (make-hash-table :test 'equal)))
    (declare (special hooks:*current-run-id*
                       tools:*file-read-state*
                       tools:*todos*
                       sf:*fetch-history*
                       sf:*fetch-raw-cache*
                       *subagent-usage*
                       *subagents*))
    (loop
      (incf n)
      (when (> n max-iterations)
        (when verbose
          (format t "~&[operandi] hit max-iterations ~A; stopping~%"
                  max-iterations))
        (return (values "[max-iterations exceeded]" messages n
                        (llm:usage-incf (llm:copy-usage usage) *subagent-usage*))))
      ;; Keep the context under the token budget BEFORE every send, so a
      ;; turn that just appended a huge tool result gets compacted before
      ;; it can blow the model's window.
      (setf messages (maybe-compact messages verbose))
      (let* ((parsed (do-chat-with-retries messages tools-vec :verbose verbose))
             (msg (extract-message parsed))
             (tcs (message-tool-calls msg))
             (text (msg-text msg)))
        ;; Fold this turn's usage (calls/tokens/cost) into the run total, and
        ;; calibrate the token estimator against the prompt_tokens the API
        ;; just reported for the messages we sent. SKIP a 200-with-error-body:
        ;; OpenRouter echoes a `usage.cost` for the prompt tokens it ingested
        ;; even when it REFUSES the generation (exhausted grant, etc.) and then
        ;; bills nothing — folding it fabricates a charge that never happened
        ;; (the reported-cents / no-actual-charge bug). A refused turn is free.
        (unless (response-has-error-body-p parsed)
          (llm:fold-usage usage parsed))
        (let ((u (and (hash-table-p parsed) (gethash "usage" parsed))))
          (when (hash-table-p u)
            (let ((pt (gethash "prompt_tokens" u)))
              (when (integerp pt)
                (calibrate-chars-per-token
                 (reduce #'+ messages :key #'msg-chars :initial-value 0) pt)))))
        ;; Some providers (or rare 200-with-empty-body responses) yield
        ;; nil msg. Bail rather than crashing on (gethash _ nil).
        ;; Print the raw parsed response so we can diagnose; OpenRouter
        ;; sometimes embeds an 'error' object in a 200 body.
        (when (null msg)
          (when verbose
            (format t "~&[operandi] empty assistant message; raw parsed: ~A~%"
                    (subseq (handler-case (jzon:stringify parsed)
                              (error () (princ-to-string parsed)))
                            0 (min 800
                                   (length
                                    (handler-case (jzon:stringify parsed)
                                      (error () (princ-to-string parsed))))))))
          (return (values (or (provider-error-text parsed) "[empty response from model]")
                          messages n
                          (llm:usage-incf (llm:copy-usage usage) *subagent-usage*))))
        ;; Append the assistant turn no matter what — the protocol
        ;; requires it.
        (setf messages (append messages (list (assistant-msg-from-response msg))))
        (cond
          (tcs
           (setf empty-turns 0)   ; a tool-calling turn is progress, not a stall
           (when verbose
             (loop for tc in tcs do
                   (format t "~&[operandi] tool: ~A(~A)~%"
                           (tool-call-name tc)
                           (let* ((fn (gethash "function" tc))
                                  (raw (and fn (gethash "arguments" fn))))
                             (subseq (or raw "") 0 (min 80 (length (or raw ""))))))))
           ;; Execute every tool_call, append each result. If the budget
           ;; cap is hit, short-circuit with a synthetic result so the
           ;; model knows to stop searching and finalize.
           (loop for tc in tcs
                 for tcid = (gethash "id" tc)
                 for name = (tool-call-name tc)
                 for (args argerr) = (multiple-value-list (parse-tool-args tc))
                 for over-budget = (and *max-tool-calls*
                                        (>= tool-call-count *max-tool-calls*))
                 for result = (cond
                                (argerr
                                 (format nil
                                         "Your ~A call was NOT executed: its ~
                                          arguments did not parse (~A). They ~
                                          were almost certainly cut off by the ~
                                          output-token limit. Retry with a ~
                                          smaller payload — write the file in ~
                                          sections (one Write for the first ~
                                          chunk, then Edit to append), and keep ~
                                          any single call under a few hundred ~
                                          lines."
                                         name argerr))
                                (over-budget
                                 (format nil
                                         "Tool budget reached (~A calls). ~
                                          No further tool calls will be ~
                                          executed; finalize your answer now."
                                         *max-tool-calls*))
                                (t (tools:invoke-tool name args)))
                 do (incf tool-call-count)
                    (when (and argerr verbose)
                      (format t "~&[operandi] TRUNCATED tool args for ~A (~A); ~
                                 telling the model to retry smaller~%"
                              name argerr))
                    (when (and over-budget verbose)
                      (format t "~&[operandi] tool budget exhausted; injecting stop~%"))
                    (setf messages
                          (append messages
                                  (list (tool-result-msg tcid result)))))
           ;; Continue loop (compaction happens at the top of the next
           ;; iteration, before the next send).
           )
          (text
           ;; Real final answer; we're done.
           (when verbose (format t "~&[operandi] done after ~A iter~%" n))
           (return (values text messages n
                           (llm:usage-incf (llm:copy-usage usage) *subagent-usage*))))
          ((equal (response-finish-reason parsed) "length")
           ;; Not a stall — the model was CUT OFF at the output cap, and on a
           ;; reasoning model the trace can eat the whole budget before any
           ;; content appears. Say so and let it try again; counting this as
           ;; an empty turn would end the run over a budget problem.
           (when verbose
             (format t "~&[operandi] turn truncated at the output cap (~A tok); retrying~%"
                     *do-chat-max-tokens*))
           (setf messages
                 (append messages
                         (list (ht "role" "user" "content"
                                   (format nil
                                           "Your last turn was cut off at the ~A-token ~
                                            output limit before you produced anything ~
                                            usable. Think less and act: take the single ~
                                            next step, and keep each tool call's payload ~
                                            well under that limit."
                                           *do-chat-max-tokens*))))))
          ((< empty-turns *max-empty-turns*)
           ;; No tool call AND no text — the model stalled (often a
           ;; null-content message). Nudge it back to work instead of
           ;; ending the run on an empty/"NULL" answer.
           (incf empty-turns)
           (when verbose
             (format t "~&[operandi] empty turn (~A/~A); nudging~%"
                     empty-turns *max-empty-turns*))
           (setf messages
                 (append messages
                         (list (ht "role" "user" "content"
                                   "Your last message was empty. Either call a tool to make progress, or write your final answer now as plain text.")))))
          (t
           ;; Still empty after nudging — salvage the last real content
           ;; rather than returning "NULL"/"" as the answer.
           (when verbose
             (format t "~&[operandi] gave up after ~A empty turns; salvaging~%"
                     empty-turns))
           (return (values (or (last-substantive messages)
                               "[no answer: the model returned only empty turns]")
                           messages n
                           (llm:usage-incf (llm:copy-usage usage) *subagent-usage*)))))))))
