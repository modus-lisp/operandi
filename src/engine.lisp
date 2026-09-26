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
  ;; #+QUICKLISP: the .asd already loads these; this is for loading the file by hand.  Guarded
  ;; because READING `ql:' is an error in an image without Quicklisp, before anything runs.
  #+quicklisp (ql:quickload '(:com.inuoe.jzon :babel) :silent t))

(defpackage #:operandi.engine
  (:use #:cl)
  (:local-nicknames (#:http  #:operandi.http)
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
           #:*live-messages* #:*on-progress* #:*steer-fn* #:message-tool-calls #:tool-result-msg
           #:*on-compact* #:*compaction-max-tokens* #:apply-backend-defaults
           #:*context-token-budget* #:*context-headroom* #:compact-threshold
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

(defparameter *max-iterations* (%env-int "OPERANDI_MAX_ITERS" 500)
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
   compaction evicts earlier findings faster than it can act on them.

   This is a CEILING, not the trigger: compaction fires at COMPACT-THRESHOLD,
   which is this less *CONTEXT-HEADROOM*.")

(defparameter *context-headroom*
  (let ((e (uiop:getenv "OPERANDI_CONTEXT_HEADROOM")))
    (or (and e (ignore-errors (let ((v (read-from-string e)))
                                (and (realp v) (< -0.001 v 0.9) (float v 1.0)))))
        0.2))
  "Fraction of *CONTEXT-TOKEN-BUDGET* kept free, so compaction fires with room
   to spare instead of at the line.

   Compacting exactly AT the budget means the send immediately before it is the
   largest one the run will ever make, and the trigger is an ESTIMATE — a
   character count divided by a calibrated ratio, not the model's tokenizer.
   Estimate low by a few percent at the moment the history is already full and
   the request is rejected for length, which is the one failure that loses the
   turn rather than degrading it.  A margin costs a slightly earlier compaction
   and buys never discovering the error from the provider.

   What it is NOT for is the reply: *CONTEXT-TOKEN-BUDGET* is already meant to
   sit at roughly half the model's window, so the answer's room comes out of the
   gap between budget and window, not out of this.  This margin covers the two
   things that happen BETWEEN checks — a single tool result arriving several
   thousand tokens larger than anything before it, and the estimator drifting
   against a tokenizer it cannot see.  0.2 of 96k is ~19k, which absorbs a large
   file read; 0.2 of a 24k local budget is ~4.8k, which absorbs a normal one.")

(defun compact-threshold ()
  "The estimated-token count at which compaction fires: the budget less its
   headroom.  Compaction also TARGETS this number rather than the budget, so a
   pass leaves the margin it was supposed to create instead of landing on the
   line and re-firing on the next tool result."
  (max 1000 (floor (* *context-token-budget* (- 1.0 *context-headroom*)))))

(defvar *steer-fn* nil
  "A function of no arguments, bound by the HOST, called once per iteration just
   before the next send.  Anything it returns as a non-blank string is appended
   to the running conversation as a user turn — so a message written while the
   agent is working lands INSIDE the turn and redirects it, instead of waiting
   behind it.

   This is the only way a host can get a word in edgeways: the loop is otherwise
   a closed cycle of send / tool / send, and a queued message cannot be seen
   until the turn it is queued behind has finished — which, for a long run, is
   exactly when it is no longer the thing you wanted to say.

   Called between a tool-result append and the next send, so the history it
   appends to always ends in a complete assistant/tool exchange.  Errors from it
   are swallowed: steering is a convenience, and a host bug in it must not take
   down a run that is otherwise fine.")

(defvar *on-progress* nil
  "Optional (lambda (messages)) called whenever the in-flight message list
   grows during a run — after every tool result and every compaction — so a
   host can checkpoint a long turn to disk. A 53-minute turn that dies at
   minute 52 otherwise leaves nothing: sessions were only persisted when a
   turn completed.")

(defun note-progress (messages)
  (setf *live-messages* messages)
  (when *on-progress* (ignore-errors (funcall *on-progress* messages)))
  messages)

(defvar *live-messages* nil
  "The running message list of the RUN in progress, updated after every
   append/compaction. Lets a host that aborts a turn (Ctrl-C in the TUI)
   salvage what the agent did before the interrupt instead of dropping it.")

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

(defparameter *image-tokens* 1500
  "Estimated prompt tokens per attached image. Providers bill a screenshot
   at roughly 1-2k regardless of its base64 length, which is what the
   content string would otherwise be measured by.")

(defun msg-chars (m)
  "Character weight of one message: content + tool_call arguments + a small
   per-message overhead. Images count a fixed *IMAGE-TOKENS* each."
  (let ((chars 4))
    (let ((c (gethash "content" m)))
      (incf chars (length (llm:content-text c)))
      (incf chars (round (* (llm:content-images c) *image-tokens* *chars-per-token*))))
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
  "You are compacting the context of an autonomous coding agent so it can
continue a task in a fresh context window with nothing else to go on. Below
is a span of its conversation: assistant messages, tool calls, tool results,
and any user messages. It may also contain an earlier compaction brief —
if so, carry everything from it forward, merged with what came after.

Write a CONTINUATION BRIEF with exactly these sections, in this order. It
replaces the span entirely; anything you leave out is gone. Use the agent's
own file paths, symbol names, commands, and error text verbatim — never
paraphrase an identifier. The short sections come first so they survive
even if the output is cut off; keep them tight and put the bulk in 9.

## 1. Primary request and intent
What the user asked for, including constraints and preferences they
expressed along the way. Quote them where the wording matters.

## 2. All user messages
Every user message in the span, verbatim, in order. Skip only synthetic
nudges from the runtime (they begin \"Your last\").

## 3. Pending tasks
What the user asked for that is not yet done, in order.

## 4. Current work
Exactly what was in progress at the end of the span: which file, which
function, what the last tool call was and what came back.

## 5. Next step
The single concrete action that continues the current work — and only if
it follows directly from the span; do not invent new work.

## 6. Key technical concepts
Technologies, libraries, architectures, conventions of this codebase the
agent needed to know.

## 7. Errors and how they were resolved
Each error hit, what it turned out to be, what fixed it. Include user
corrections of the agent's approach — those are the most expensive things
to relearn.

## 8. Problem solving
What was established as fact (measurements, test results, confirmed
behaviors) and what was ruled out.

## 9. Files and code
Every file read, created, or edited: full path, why it matters, and the
CURRENT state of what changed — not a replay of each edit. Include the
code the next step depends on (the exact snippet, not a description);
summarize the rest.")

(defparameter *compaction-merge-prompt*
  "Below are several continuation briefs, each covering a consecutive span
of the same agent conversation, in order. Merge them into ONE brief with
the same nine numbered sections, keeping every file path, snippet, error,
decision, and user message from all of them. Later spans supersede earlier
ones where they conflict (a task that was pending and then completed is
completed). Do not shorten for brevity — nothing may be lost.")

(defparameter *compaction-max-tokens* 8192
  "Output cap for one summarizer call. The brief has nine sections and
   quotes code, and on OpenRouter a reasoning model's thinking counts
   against this too; 4096 cut section 3 off mid-snippet.")

(defun render-turn-for-summary (m)
  "One message as the summarizer sees it: role, text, and each tool call as
   name(args) — the call is what tells the reader which file was touched."
  (let* ((role (gethash "role" m))
         (raw  (gethash "content" m))
         (content (llm:content-text raw))
         (tcs (gethash "tool_calls" m)))
    (with-output-to-string (s)
      (format s "[~A] ~A~%" role
              ;; Tool results are bounded by the Read budget already; this
              ;; is a backstop for pathological ones.
              (if (> (length content) 24000)
                  (concatenate 'string (subseq content 0 24000) "...[truncated]")
                  content))
      (when (and tcs (or (vectorp tcs) (listp tcs)))
        (map nil (lambda (tc)
                   (let ((fn (and (hash-table-p tc) (gethash "function" tc))))
                     (when (hash-table-p fn)
                       (format s "    -> ~A(~A)~%" (gethash "name" fn) (gethash "arguments" fn)))))
             (if (listp tcs) (coerce tcs 'vector) tcs))))))

(defun chunk-turns (turns max-tokens)
  "Split TURNS into consecutive chunks each estimated at or under MAX-TOKENS
   (a single oversized message is a chunk by itself). The summarizer has the
   same window as the agent, so a middle that overflowed the budget can't be
   sent to it in one piece."
  (let ((chunks '()) (cur '()) (cur-tok 0))
    (dolist (m turns)
      (let ((tok (estimate-tokens (list m))))
        (when (and cur (> (+ cur-tok tok) max-tokens))
          (push (nreverse cur) chunks)
          (setf cur '() cur-tok 0))
        (push m cur) (incf cur-tok tok)))
    (when cur (push (nreverse cur) chunks))
    (nreverse chunks)))

(defun summarize-chunk (turns prompt)
  "One summarizer call over TURNS (or, with the merge prompt, over strings).
   Returns the text, or NIL on error."
  (let ((rendered (with-output-to-string (s)
                    (dolist (m turns)
                      (write-string (if (stringp m) m (render-turn-for-summary m)) s)
                      (terpri s)))))
    (handler-case
        (values (llm:llm-chat rendered :system prompt
                                       :max-tokens *compaction-max-tokens*
                                       :temperature 0.0
                                       :effort :low))
      (error () nil))))

(defun summarize-turns (turns)
  "Compress a list of message hash-tables into a continuation brief (see
   *COMPACTION-PROMPT*). The span is summarized in window-sized chunks and,
   if there was more than one, the partial briefs are merged by a second
   pass. Returns the brief as a string, or NIL if every call failed."
  (let* ((chunk-tokens (max 4000 (floor *context-token-budget* 2)))
         (chunks (chunk-turns turns chunk-tokens))
         (partials (remove nil (mapcar (lambda (c) (summarize-chunk c *compaction-prompt*))
                                       chunks))))
    (cond ((null partials) nil)
          ((null (rest partials)) (first partials))
          (t (or (summarize-chunk
                  (loop for p in partials for i from 1
                        collect (format nil "=== Brief ~D of ~D ===~%~A~%" i (length partials) p))
                  *compaction-merge-prompt*)
                 ;; merge failed: concatenating loses nothing, just costs tokens
                 (format nil "~{~A~^~%~%~}" partials))))))

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
                (llm:content-text c))
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

(defparameter *stall-repeat* 4
  "How many times the SAME (tool, arguments) pair may repeat before the run is
   treated as stalled. Four rather than two: a retry is normal, a second look at
   a file after editing it is normal, and a legitimate sweep can read the same
   path twice. Four identical calls in a row is not a sweep.")

(defparameter *stall-window* 12
  "How many recent tool calls the no-progress check looks at. If a window this
   long contains no call the run has not already made, nothing new is being
   touched.")

(defun %tool-call-sig (tc)
  "A tool call as (name . arguments), which is what `the same call again' means."
  (let ((fn (gethash "function" tc)))
    (and fn (cons (gethash "name" fn) (gethash "arguments" fn)))))

(defun %stall-reason (sigs)
  "SIGS is every tool-call signature this run has issued, oldest first. Returns a
   sentence naming what looks stuck, or NIL.

   WHY STRUCTURE AND NOT A MODEL. A count of iterations cannot tell a long job
   from a stuck one, which is the whole defect this replaces — but repetition
   can, it costs nothing, and it is not a judgement that can be wrong in an
   interesting way. Only the shapes below stop a run; anything subtler is left
   to the iteration backstop rather than guessed at."
  (let ((n (length sigs)))
    (cond
      ((< n *stall-repeat*) nil)
      ;; the classic spin: the same call, with the same arguments, over and over
      ((let ((tail (last sigs *stall-repeat*)))
         (and (every (lambda (x) (equal x (first tail))) tail)
              (car (first tail))))
       (format nil "the last ~D tool calls were all ~A with identical arguments"
               *stall-repeat* (car (car (last sigs)))))
      ;; nothing new in a long while: every call in the window is one already made
      ((and (>= n (* 2 *stall-window*))
            (let* ((window (last sigs *stall-window*))
                   (earlier (subseq sigs 0 (- n *stall-window*))))
              (every (lambda (x) (member x earlier :test #'equal)) window)))
       (format nil "the last ~D tool calls repeat work already done — nothing new ~
                    has been touched" *stall-window*))
      (t nil))))

(defparameter +brief-marker+ "[Context was compacted:"
  "How a compaction brief announces itself.  OFFLOAD-MIDDLE writes it and
   RENDER-USER-TURNS reads it, so the two must not drift apart.")

(defparameter +instructions-heading+ "## User instructions from the compacted span"
  "Heading of the verbatim-instruction block inside a brief, so a LATER
   compaction can find it and carry those instructions forward.")

(defun %carried-instructions (brief)
  "The bullet lines of BRIEF's verbatim-instruction block.

   A brief is appended with role USER by design — it reads as something to
   continue from rather than as the model's own prior claim.  The cost is that
   the NEXT compaction sees it as a user turn: without this, the new brief's
   instruction list was the old brief's HEADER, one line, and every real
   instruction the operator had given vanished from the list while the brief
   itself doubled by nesting.  Measured on a live session: 7,045 -> 14,514
   tokens in one pass, with the live task no longer in the list at all."
  (let ((at (search +instructions-heading+ brief)))
    (when at
      (let ((lines '()))
        (with-input-from-string (in (subseq brief at))
          (read-line in nil)                        ; the heading itself
          (loop for l = (read-line in nil)
                while l
                do (let ((tl (string-left-trim '(#\Space #\Tab) l)))
                     (cond ((uiop:string-prefix-p "- " tl) (push (subseq tl 2) lines))
                           ((zerop (length tl)))     ; blank lines inside the block
                           (t (return))))))          ; the next section starts
        (nreverse lines)))))

(defun %bare-continuation-p (c)
  "True for a user turn that carries no task of its own — only permission to keep
   going. These are answers to an interruption, not instructions, and they mean
   nothing without the interruption beside them."
  (member (string-downcase (string-trim " .!?" c))
          '("continue" "go on" "keep going" "proceed" "carry on" "resume" "continue.")
          :test #'string=))

(defun %first-sentence (s)
  "S up to its first period, trimmed — enough of an engine nudge to say what it was."
  (let* ((s (string-trim '(#\Space #\Newline #\Tab) s))
         (dot (position #\. s)))
    (if dot (subseq s 0 dot) s)))

(defun render-user-turns (turns)
  "The user messages in TURNS, verbatim, as a block to carry across compaction.
   The summarizer is told to report facts, not requirements — so instructions
   given mid-session (\"remove the pin too\", \"drop the carto layers\") were
   surviving only as whatever the summary happened to quote. They're short and
   they ARE the task; keep them word for word. Synthetic user nudges from the
   engine (see the output-cap / empty-turn paths) start with \"Your last\" and
   are skipped, and an earlier BRIEF is not an instruction either — its own list
   is lifted into this one instead (see %CARRIED-INSTRUCTIONS)."
  (let ((users '())
        (note nil))                     ; the engine nudge most recently seen, if any
    (dolist (m turns)
      (let ((c (llm:content-text (gethash "content" m))))
        (when (and (equal (gethash "role" m) "user") (plusp (length c)))
          (cond
            ;; An engine nudge is not an instruction, so it does not get a line of
            ;; its own — but it is the REASON for any bare "continue" after it, so
            ;; hold on to it rather than dropping it on the floor.
            ((uiop:string-prefix-p "Your last" c) (setf note c))
            ;; A previous brief is not an instruction either — but it CONTAINS
            ;; them, so lift its list into this one and let the instructions
            ;; survive however many compactions the run takes.
            ((uiop:string-prefix-p +brief-marker+ c)
             (dolist (i (%carried-instructions c)) (push i users))
             (setf note nil))
            ;; "continue" alone says nothing about WHAT to continue.  Carried bare
            ;; into the brief it reads as one more instruction competing with the
            ;; real ones above it, and the model picks up whichever it likes --
            ;; which in practice was the oldest.  Pair it with the interruption it
            ;; was answering and it says what it always meant.
            ((%bare-continuation-p c)
             (push (if note
                       (format nil "~A  [answering an interruption: ~A]" c (%first-sentence note))
                       (format nil "~A  [the previous turn was interrupted]" c))
                   users)
             (setf note nil))
            (t (push c users) (setf note nil))))))
    (setf users (nreverse users))
    (when users
      (format nil "## User instructions from the compacted span (verbatim, in order; ~
                   the LAST one is the live task)~%~{  - ~A~%~}"
              users))))

(defun render-files-touched (turns)
  "Paths named in Read/Edit/Write/Glob/Grep calls across TURNS, deduplicated
   in first-seen order. The contents were in the tool results being dropped;
   the next Edit needs a fresh Read anyway (the run's read-guard still holds),
   so tell the agent which files those were rather than let it rediscover."
  (let ((paths '()))
    (dolist (m turns)
      (let ((tcs (gethash "tool_calls" m)))
        (when (and tcs (or (vectorp tcs) (listp tcs)))
          (map nil (lambda (tc)
                     (let* ((fn (and (hash-table-p tc) (gethash "function" tc)))
                            (raw (and (hash-table-p fn) (gethash "arguments" fn)))
                            (args (and (stringp raw) (ignore-errors (jzon:parse raw)))))
                       (when (hash-table-p args)
                         (let ((p (or (gethash "path" args) (gethash "file_path" args))))
                           (when (and (stringp p) (plusp (length p)))
                             (pushnew p paths :test #'string=))))))
               (if (listp tcs) (coerce tcs 'vector) tcs)))))
    (when paths
      (format nil "## Files touched in the compacted span (re-Read before editing)~%~{  - ~A~%~}"
              (reverse paths)))))

(defun apply-backend-defaults ()
  "Re-derive backend-dependent defaults after the model/backend changes:
   the 24k compaction budget is sized for a local llama; a frontier model
   through OpenRouter gets 96k. An explicit OPERANDI_CONTEXT_BUDGET wins."
  (unless (uiop:getenv "OPERANDI_CONTEXT_BUDGET")
    (setf *context-token-budget*
          (if (eq llm:*llm-backend* :openrouter) 96000 24000))))

(defvar *last-offload-path* nil
  "Set by OFFLOAD-MIDDLE for the *ON-COMPACT* hook; NIL after a tier-1-only pass.")

(defvar *on-compact* nil
  "Optional (lambda (before-tokens after-tokens offload-path)) called after a
   compaction, so a host UI can show it happened — the TUI runs with VERBOSE
   off, and a silent context reset is exactly the kind of thing a user should
   see.")

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
             (setf *last-offload-path* path)
             (append head
                     ;; USER role, not assistant: the brief reads as
                     ;; instructions to continue from, and models weight a
                     ;; user turn as ground truth where an assistant turn
                     ;; reads as their own possibly-wrong prior claim.
                     (list (ht "role" "user"
                               "content"
                               (format nil "~a ~D messages replaced by the brief below~@[; the raw messages are OFFLOADED (not lost) to ~A — Read that file if you need a detail the brief dropped~]. Continue the work from where the brief leaves off; do not re-derive what it establishes.]~@[~%~%~A~]~@[~%~%~A~]~@[~%~%~A~]~@[~%~%~A~]"
                                       +brief-marker+ (length middle) path
                                       (render-pinned-todos)
                                       (render-user-turns middle)
                                       (render-files-touched middle)
                                       summary)))
                     tail)))))))

(defun compact-messages (messages)
  "Bring MESSAGES under *CONTEXT-TOKEN-BUDGET*. Tier 1: trim old large
   tool results (cheap, no LLM, structure-preserving). Tier 2, only if
   still over budget: OFFLOAD the middle (reversibly — raw kept in a file)
   with a summary pointer. Returns a list at or below budget where
   possible; never grows the input."
  (if (<= (estimate-tokens messages) (compact-threshold))
      messages
      (let ((trimmed (trim-old-tool-results messages *compact-keep-last*)))
        (if (<= (estimate-tokens trimmed) (compact-threshold))
            trimmed
            (offload-middle trimmed)))))

(defun maybe-compact (messages verbose)
  "Compact MESSAGES if it's over the token budget; else return it as-is."
  (if (<= (estimate-tokens messages) (compact-threshold))
      messages
      (let ((out (compact-messages messages)))
        (when verbose
          (format t "~&[operandi] compacted ~D->~D tok (~D->~D msgs)~%"
                  (estimate-tokens messages) (estimate-tokens out)
                  (length messages) (length out)))
        (when *on-compact*
          (ignore-errors
           (funcall *on-compact* (estimate-tokens messages) (estimate-tokens out)
                    (shiftf *last-offload-path* nil))))
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
  * The user can attach images. An attached image is delivered INSIDE the
    message, as an image you can see directly; the text marks where with
    [attached image N]. Look at it and answer from it. Do not search the
    filesystem for it, read it as a file, or decode it with tools — none
    of that is needed and the bytes on disk tell you less than your eyes.

Working discipline (this is how you avoid thrashing):
  * PLAN first. For any multi-step task, write a short TodoWrite plan naming
    the SPECIFIC files and functions you will change and the approach. Your
    todos are pinned across compaction — they are your durable memory, so
    record findings there (a file:line, the exact edit you intend), and
    update them as you go. Don't investigate past what the plan needs.
  * USE THE FILE TOOLS, not the shell, to look at code. Read (view a file or a
    region), Grep (search), and Glob (find files) each take an absolute or
    relative path and hand back exactly what you need. Do NOT inspect code with
    Bash `sed -n`, `grep`, `cat`, `head`, or `find`: every one is a full
    round-trip that buys a single slice and teaches you nothing the file tools
    wouldn't, and stringing dozens of them together is how a task runs out of
    steps before it is done. Bash is for RUNNING things — builds, tests, git,
    the app — not for reading them.
  * READ narrowly but in FULL REGIONS. Locate code with Grep, then Read the
    whole enclosing function/section in one call with offset/limit — a generous
    region you'll actually reason over beats ten peeks at five lines each. Read
    bounds its own output and tells you the size of any middle it elided.
  * Don't repeat work. Never re-run a search you've already run or re-read a
    file you've already seen — consult your todos/notes instead. If you catch
    yourself re-reading, you've lost the plan; rebuild it from your todos.
  * ANSWERING A QUESTION IS DIFFERENT FROM DOING A TASK. When you are asked
    why something happens, what is causing it, or whether something is true,
    do not start grinding through it one tool call at a time. First ask what
    would have to be TRUE for each candidate explanation, and write those
    down as separate claims that evidence could REFUTE — \"the proxy sends no
    cache header\", not \"look at caching\". Then:
      - Findings first. Someone may have settled one already; re-deriving a
        proved claim costs a whole run and usually reaches a worse answer.
      - Investigate the rest, in ONE call, all claims at once. Each goes to
        its own worker that reports a verdict with evidence, and they run in
        parallel — so four claims cost about what one costs serially, and
        you read four verdicts instead of four transcripts.
    A question that splits into independent claims and is ground out serially
    anyway is the most common way a run burns its whole budget and still ends
    up guessing. Investigating your own single best guess is the same mistake
    in miniature: list the rival explanations too, or you will confirm the
    first thing you thought of.
  * KNOW WHAT REGENERATES, before you delete, overwrite or truncate anything.
    DISPOSABLE: caches, build output, .fasl and core images, node_modules,
    scratch and temp trees, offloaded context. Losing these costs time, not
    information — they rebuild from something that still exists.
    DURABLE: source, git history, notes and ledgers, session transcripts,
    anything a person typed, and anything fetched that cannot be fetched
    again. Losing these costs information, and no apology restores it.
    Anything you cannot confidently place in the first group belongs in the
    second. Say which one it is in the sentence where you propose the
    deletion; if you cannot name it, you do not understand the command yet.
  * AIM A DESTRUCTIVE TEST BEFORE YOU FIRE IT. Exercising code that deletes,
    evicts, truncates or overwrites is where agents destroy real data — not
    by deciding to, but by running a sweep they believed was pointed at a
    fixture. Config resolves quietly: a root taken from the loaded file's own
    path, a directory from an env var, a default that is the live system.
    So PROVE the target first — print the resolved absolute path the code
    will act on, confirm it is your scratch tree, and only then run it.
    \"I set it to a temp dir\" is not proof; the printed path is.
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
    ;; Reasoning: /effort, --effort, OPERANDI_EFFORT (see LLM:*LLM-EFFORT*);
    ;; the legacy *DO-CHAT-DISABLE-REASONING* still means :OFF if nothing
    ;; explicit was chosen.
    (llm:apply-reasoning body (or llm:*llm-effort*
                                  (and *do-chat-disable-reasoning* :off)))
    (when (eq llm:*llm-backend* :openrouter)         ; cost/token accounting
      (setf (gethash "usage" body) (ht "include" t)))
    (when llm:*llm-model* (setf (gethash "model" body) llm:*llm-model*))
    body))

(defun do-chat-blocking (messages tools-vec &key (max-tokens *do-chat-max-tokens*)
                                                 (temperature 0.0))
  "One blocking chat completion. Returns the parsed top-level hash table."
  (let ((resp (http:post llm:*llm-url*
                         :content (with-output-to-string (s)
                                    (jzon:stringify (build-chat-body messages tools-vec
                                                                     max-tokens temperature nil)
                                                    :stream s))
                         :headers (chat-headers)
                         :read-timeout llm:*llm-read-timeout*)))
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
  (let ((stream (http:post llm:*llm-url*
                           :content (with-output-to-string (s)
                                      (jzon:stringify (build-chat-body messages tools-vec
                                                                       max-tokens temperature t)
                                                      :stream s))
                           :headers (chat-headers)
                           :read-timeout llm:*llm-read-timeout*
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
  "Turn an http:http-request-failed into an error-body parsed ({error:{message,code}}),
   preferring the provider's own JSON error message from the response body. So a
   404/402/401 surfaces its real reason via provider-error-text rather than a bare
   '[empty response from model]'. Non-retryable 4xx are marked so the loop stops."
  (let* ((code (ignore-errors (http:response-status e)))
         (raw (ignore-errors (http:response-body e)))
         (body (cond ((stringp raw) raw)
                     ((typep raw '(vector (unsigned-byte 8)))
                      (ignore-errors (babel:octets-to-string raw :encoding :utf-8 :errorp nil)))
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
                       (http:http-request-failed (e) (http-error->parsed e)))))
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
                       (http:http-request-failed (e)
                         ;; A real HTTP 4xx/5xx (e.g. OpenRouter 404 "No allowed
                         ;; providers", 402 grant, 401 bad key). dex discards the
                         ;; body into the condition — recover it as an error-body
                         ;; parsed so provider-error-text surfaces the REAL reason
                         ;; instead of a mysterious "[empty response from model]".
                         (when verbose
                           (format t "~&[operandi] http ~A (try ~A)~%"
                                   (ignore-errors (http:response-status e)) (1+ attempt)))
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
         (call-sigs '())      ; every tool call issued this run, newest first
         (stall-warned nil)   ; the one free correction has been spent
         (usage (llm:make-usage))
         (*subagent-usage* (llm:make-usage))
         (*subagents* (make-hash-table :test 'equal))
         (sf:*fetch-history* (make-hash-table :test 'equal))
         (sf:*fetch-raw-cache* (make-hash-table :test 'equal))
         (*live-messages* messages))
    (declare (special hooks:*current-run-id*
                       tools:*file-read-state*
                       tools:*todos*
                       sf:*fetch-history*
                       sf:*fetch-raw-cache*
                       *subagent-usage*
                       *subagents*))
    ;; Checkpoint the prompt itself before the first model call: a session
    ;; should exist on disk from the first real user message, not from the
    ;; first tool result.
    (note-progress messages)
    (loop
      (incf n)
      (when (> n max-iterations)
        (when verbose
          (format t "~&[operandi] hit max-iterations ~A; stopping~%"
                  max-iterations))
        ;; SAY SO IN THE HISTORY, not only in the return value.  The reply string
        ;; goes to whoever called RUN; the MESSAGES go on to the next turn, and
        ;; without this the model's view is a tool result followed, for no stated
        ;; reason, by the operator saying "continue" — so it guesses what to
        ;; continue, and guessing wrong looks like it forgot the task.  The
        ;; output-cap path below already does this; the iteration cap is the same
        ;; kind of interruption and deserves the same sentence.  "Your last"
        ;; prefix is load-bearing: RENDER-USER-TURNS keys on it to tell an engine
        ;; nudge from something the operator actually asked for.
        (setf messages
              (append messages
                      (list (ht "role" "user" "content"
                                (format nil
                                        "Your last turn was stopped at the ~A-iteration cap ~
                                         before it finished. Nothing is wrong and nothing was ~
                                         lost — the work is simply unfinished. Pick up from the ~
                                         last completed step; say what remains before continuing ~
                                         it." max-iterations)))))
        ;; ...and hand back a history that is under budget, like every other exit
        ;; from this loop.  The compaction below runs before each SEND, so bailing
        ;; out here used to be the one path that returned an uncompacted history
        ;; for the caller to persist.
        (setf messages (maybe-compact messages verbose))
        (return (values "[max-iterations exceeded]" messages n
                        (llm:usage-incf (llm:copy-usage usage) *subagent-usage*))))
      ;; STUCK, NOT LONG.  The iteration cap above is a runaway-cost backstop and
      ;; nothing more: it fires on length, and length is a bad proxy for trouble --
      ;; a real job trips it while a four-call spin never does.  This is the check
      ;; that actually means something, and it gets ONE free correction first,
      ;; because naming the loop is usually enough for the model to leave it (the
      ;; output-cap path works the same way).  A second trip stops the run and
      ;; says what looked stuck, which beats "[max-iterations exceeded]".
      (let ((reason (%stall-reason (reverse call-sigs))))
        (when reason
          (cond
            ((not stall-warned)
             (setf stall-warned t)
             (when verbose
               (format t "~&[operandi] possible stall: ~A; nudging~%" reason))
             (setf messages
                   (append messages
                           (list (ht "role" "user" "content"
                                     (format nil
                                             "Your last turns look stuck: ~A. Stop and say, in ~
                                              one line, what you are trying to establish and why ~
                                              the repeat did not settle it — then either do ~
                                              something different or give your final answer."
                                             reason))))))
            (t
             (when verbose
               (format t "~&[operandi] stalled: ~A; stopping~%" reason))
             (setf messages
                   (append messages
                           (list (ht "role" "user" "content"
                                     (format nil
                                             "Your last turn was stopped because the run stalled: ~
                                              ~A. Nothing is wrong and nothing was lost — the work ~
                                              is simply unfinished. Pick up from the last ~
                                              completed step; say what remains before continuing ~
                                              it." reason)))))
             (setf messages (maybe-compact messages verbose))
             (return (values (format nil "[stalled: ~A]" reason) messages n
                             (llm:usage-incf (llm:copy-usage usage) *subagent-usage*)))))))
      ;; STEERING.  Before compaction, so anything just said is part of what gets
      ;; measured and sent rather than arriving after the window was sized.
      (when *steer-fn*
        (let ((s (ignore-errors (funcall *steer-fn*))))
          (when (and (stringp s) (plusp (length (string-trim '(#\Space #\Newline #\Tab) s))))
            (when verbose (format t "~&[operandi] steer: ~A~%" s))
            (setf messages (append messages (list (ht "role" "user" "content" s)))))))
      ;; Keep the context under the token budget BEFORE every send, so a
      ;; turn that just appended a huge tool result gets compacted before
      ;; it can blow the model's window.
      (setf messages (note-progress (maybe-compact messages verbose)))
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
        (setf messages (append messages (list (assistant-msg-from-response msg)))
              *live-messages* messages)
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
           ;; Remember what was called, so the stall check at the top of the next
           ;; iteration can see repetition.  Signatures only — no results kept.
           (dolist (tc tcs) (push (%tool-call-sig tc) call-sigs))
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
                          (note-progress
                           (append messages
                                   (list (tool-result-msg tcid result))))))
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
