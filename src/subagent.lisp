;;; src/subagent.lisp
;;;
;;; The Task tool: spawn a sub-operandi with its own clean context,
;;; let it work, return its final string answer to the parent.
;;;
;;; The whole point: a parent agent burning through a multi-step task
;;; accumulates context (tool calls, intermediate results, dead ends).
;;; Long contexts cost tokens and confuse models. Subagents reset that
;;; — each subtask runs in its own fresh conversation with its own
;;; tool subset and (optionally) its own system prompt; the parent
;;; sees only the final synthesized answer.
;;;
;;; Loaded after engine.lisp because the Task impl calls eng:run.
;;;
;;; Recursion guard: *SUBAGENT-DEPTH* tracks nesting. Limit is 3 by
;;; default — deep enough for "decompose into pieces, each piece can
;;; spawn a tiny subtask" but not deep enough to fork-bomb.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (ql:quickload :bordeaux-threads :silent t))

(defpackage #:operandi.subagent
  (:use #:cl)
  (:local-nicknames (#:llm   #:operandi.llm)
                    (#:tools #:operandi.tools)
                    (#:hooks #:operandi.hooks)
                    (#:eng   #:operandi.engine)
                    (#:jzon  #:com.inuoe.jzon)
                    (#:bt    #:bordeaux-threads))
  (:export #:*subagent-depth*
           #:*subagent-max-depth*
           #:*fan-max*
           #:*verdict*
           #:*findings-file*
           #:*worker-model*
           #:parse-worker-model))

(in-package #:operandi.subagent)

(defvar *subagent-depth* 0
  "Per-thread special: incremented when entering a subagent. The
   engine binds it freshly on each RUN call (well, indirectly — via
   the Task tool itself).")

(defparameter *subagent-max-depth* 3
  "Hard cap on subagent nesting. Beyond this Task/Fan refuse.")

(defparameter *fan-max* 8
  "Max subagents Fan runs concurrently. Extra tasks queue into batches.")

(defun parse-tool-names (tools-str)
  "Comma-separated tool names -> list, or the default toolset if blank."
  (if (or (null tools-str) (not (stringp tools-str)) (zerop (length tools-str)))
      (tools:default-tools)
      (remove "" (mapcar (lambda (s) (string-trim " " s))
                         (uiop:split-string tools-str :separator ","))
              :test #'string=)))

;;; ------------------------- the worker tier ----------------------------
;;; The loop operandi exists for splits cleanly by cost. Deciding WHAT to
;;; test — turning one question into rival, falsifiable claims — is a
;;; judgment move, and the A/B in 8edec0b showed a cheap model will not make
;;; it even holding the tool. Settling a claim is not: cheap workers do it
;;; well. So the orchestrator and its workers must be able to run on
;;; DIFFERENT models, or the architecture cannot be expressed at all — a
;;; strong orchestrator drags an expensive swarm with it, and a cheap one
;;; cannot decompose.
;;;
;;; *WORKER-MODEL* is that second tier. NIL means inherit the orchestrator's
;;; model (the old behaviour). Otherwise it is a model spec — a
;;; vendor/name OpenRouter slug, or "llama" — bound with LLM:WITH-OPENROUTER
;;; / LLM:WITH-LLAMA around each subagent's run. Those rebind with LET, and
;;; the binding is made inside the worker's own thread, so the orchestrator's
;;; global model is never touched.

(defun parse-worker-model (s)
  "\"inherit\"/\"\"/NIL -> NIL; \"llama\" or a vendor/name slug -> itself.
   Second value NIL if S is not a usable spec."
  (let ((k (and (stringp s) (string-trim " " s))))
    (cond ((or (null k) (zerop (length k)) (string-equal k "inherit") (string-equal k "none"))
           (values nil t))
          ((string-equal k "llama") (values "llama" t))
          ((and (find #\/ k) (not (find #\Space k))) (values k t))
          (t (values nil nil)))))

(defvar *worker-model*
  (let ((e (uiop:getenv "OPERANDI_WORKER_MODEL")))
    (and e (nth-value 0 (parse-worker-model e))))
  "Model the subagent tier runs on (Task, Fan, Spawn, Investigate workers),
   or NIL to inherit the orchestrator's. Seeded from OPERANDI_WORKER_MODEL;
   --worker-model and /workers set it; Investigate can override per call.")

(defun call-with-worker-model (model thunk)
  "Call THUNK with the LLM client bound to MODEL for its dynamic extent, or
   unchanged when MODEL is NIL. An unrecognised spec inherits rather than
   failing the run — a worker on the wrong model beats no worker."
  (cond ((null model) (funcall thunk))
        ((string-equal model "llama") (llm:with-llama (funcall thunk)))
        ((find #\/ model) (llm:with-openrouter (:model model) (funcall thunk)))
        (t (funcall thunk))))

(defun run-one-subagent (desc tool-names depth &optional history)
  "Run ONE subagent to completion in the current thread. With HISTORY, it
   RESUMES that conversation (DESC is ignored) instead of starting fresh —
   this is how SendMessage continues a persistent subagent. Returns a plist
   (:desc :iters :messages :usage :text) — :messages is the full (possibly
   compacted) conversation to persist; :usage is the child's full LLM:USAGE (its
   own turns plus its own nested subagents). Binds *SUBAGENT-DEPTH*
   explicitly (threads don't inherit dynamic bindings). Tool-call logging
   stays ON: the store is thread-safe (operandi.store:*db-lock*), so a
   subagent's calls are logged under its own run-id — which is exactly the
   audit trail you want when a parallel run misbehaves. Usage is returned,
   NOT added to the parent accumulator here: under Fan this runs in a
   worker thread that can't see the parent's binding, so the caller rolls
   it up."
  (handler-case
      ;; *ON-TOKEN* nil: a subagent's tokens don't stream into the parent's
      ;; live display (they'd interleave); the parent sees only its result.
      (let ((*subagent-depth* depth)
            (eng:*on-token* nil))
        (declare (special *subagent-depth*))
        (call-with-worker-model
         *worker-model*
         (lambda ()
           (multiple-value-bind (text messages iters usage)
               (eng:run desc :tool-names tool-names :verbose nil :history history)
             (list :desc desc :iters (or iters 0) :messages messages
                   :usage (or usage (llm:make-usage)) :text (or text "")
                   ;; read INSIDE the binding: the model this worker really used
                   :model (or llm:*llm-model* (string-downcase
                                               (symbol-name llm:*llm-backend*))))))))
    (error (e)
      (list :desc desc :iters 0 :messages history :usage (llm:make-usage)
            :text (format nil "SUBAGENT ERROR: ~A" e)))))

(defun roll-up-usage! (results)
  "Sum every result's usage into a fresh struct and add it to the parent
   run's accumulator (ENG:*SUBAGENT-USAGE*), which the parent thread — the
   one running this tool — still has bound. Returns the batch total."
  (let ((batch (llm:make-usage)))
    (loop for r across results when r do (llm:usage-incf batch (getf r :usage)))
    (when eng:*subagent-usage* (llm:usage-incf eng:*subagent-usage* batch))
    batch))

(defun run-fan (tasks tool-names depth)
  "Run TASKS concurrently (batched at *FAN-MAX*), preserving order.
   Returns the formatted, labeled results + a cost/iteration footer, and
   rolls the subagents' usage up into the parent run's total."
  (let ((results (make-array (length tasks) :initial-element nil)))
    (loop for start from 0 below (length tasks) by *fan-max*
          for end = (min (length tasks) (+ start *fan-max*))
          do (let ((threads
                     (loop for i from start below end
                           collect (let ((idx i) (d (nth i tasks)))
                                     (bt:make-thread
                                      (lambda ()
                                        (setf (aref results idx)
                                              (run-one-subagent d tool-names depth)))
                                      :name (format nil "operandi-fan-~D" i))))))
               (dolist (th threads) (ignore-errors (bt:join-thread th)))))
    (let ((batch (roll-up-usage! results)))
      (with-output-to-string (s)
        (let ((total-iters 0))
          (loop for r across results for i from 1
                when r
                  do (incf total-iters (getf r :iters))
                     (format s "~&=== subagent ~D (~A iters): ~A ===~%~A~%~%"
                             i (getf r :iters)
                             (let ((d (getf r :desc)))
                               (subseq d 0 (min 60 (length d))))
                             (getf r :text)))
          (format s "~&[fan: ~D subagents, ~A total iters, ~A]~%"
                  (length results) total-iters (llm:usage-summary batch)))))))

;;; ----------------------- typed verdicts -----------------------------
;;; Fan hands the orchestrator N essays and makes it read them. That is
;;; the wrong shape for the loop this is built for — an expensive model
;;; proposes hypotheses, a swarm of cheap ones settles them — because the
;;; expensive model then pays to re-read every worker's reasoning to find
;;; the one bit that matters: did it hold?
;;;
;;; So a worker does not WRITE its conclusion, it CALLS it. The Verdict
;;; tool's arguments are already JSON and already schema-checked, so the
;;; claim is structured by construction instead of scraped back out of
;;; prose. INVESTIGATE returns those records; the reasoning that produced
;;; them dies with the worker, which is the point.

(defvar *verdict* nil
  "Per-thread slot where the Verdict tool records this worker's finding,
   NIL until it calls one. Bound freshly around each investigation —
   threads do not inherit dynamic bindings, so RUN-ONE-INVESTIGATION
   binds it inside the worker thread.")

(tools:define-tool "Verdict"
    (:description "Report your verdict on the hypothesis you were given.
This is HOW YOU REPORT — calling it is the point of your run, not a
formality at the end. Call it exactly once, when you have settled the
hypothesis or established that you cannot.

Put the actual evidence in EVIDENCE: a file:line, the command you ran and
what it printed, the measurement you took. Quote specifics. An impression
(\"seems correct\", \"looks fine\") is not evidence and wastes the call."
     :schema (llm:ht
              "type" "object"
              "properties"
              (llm:ht
               "verdict" (llm:ht "type" "string"
                                 "enum" (vector "confirmed" "refuted" "undetermined")
                                 "description" "confirmed = the evidence supports the hypothesis; refuted = the evidence contradicts it; undetermined = you could not settle it. Undetermined is a real answer — do not guess to avoid it.")
               "evidence" (llm:ht "type" "string"
                                  "description" "The concrete evidence: file:line, command output, measurement. Specifics, not impressions.")
               "confidence" (llm:ht "type" "string"
                                    "enum" (vector "high" "medium" "low")
                                    "description" "How much weight the evidence actually carries."))
              "required" (vector "verdict" "evidence")))
  (let ((v (gethash "verdict" args))
        (e (gethash "evidence" args))
        (c (or (gethash "confidence" args) "medium")))
    (setf *verdict* (list :verdict v :evidence (or e "") :confidence c))
    (format nil "Verdict recorded: ~A (~A confidence). You may stop now." v c)))

(defun investigation-prompt (hypothesis context)
  "The brief handed to one worker: a single hypothesis to settle."
  (format nil "Settle ONE hypothesis and report the verdict.

HYPOTHESIS: ~A~@[

CONTEXT you have been given (take it as established; do not re-derive it):
~A~]

Gather only the evidence needed to settle this hypothesis — you are not
fixing anything, and you are not exploring beyond it. When you have
settled it, or established that you cannot, call the Verdict tool exactly
once. Calling Verdict IS your report; a run that ends without it has
produced nothing."
          hypothesis (and (stringp context) (plusp (length context)) context)))

(defun run-one-investigation (hypothesis context tool-names depth)
  "Run one worker against one HYPOTHESIS and return its record. Binds
   *VERDICT* in THIS thread so the worker's Verdict call lands here."
  (let ((*verdict* nil))
    (declare (special *verdict*))
    (let ((r (run-one-subagent (investigation-prompt hypothesis context)
                               tool-names depth)))
      (list :hypothesis hypothesis
            :verdict *verdict*          ; NIL if the worker never called it
            :iters (getf r :iters)
            :usage (getf r :usage)
            :model (getf r :model)
            :text (getf r :text)))))

(defun verdict-record (r i)
  "One investigation result as the object the orchestrator receives."
  (let ((v (getf r :verdict)))
    (llm:ht "n" i
            "hypothesis" (getf r :hypothesis)
            "verdict" (if v (getf v :verdict) "undetermined")
            "confidence" (if v (getf v :confidence) "low")
            "evidence" (if v
                           (getf v :evidence)
                           ;; No Verdict call: say so plainly and hand over the
                           ;; worker's last words rather than silently inventing
                           ;; a verdict it never reached.
                           (format nil "[worker never called Verdict] ~A"
                                   (let ((t* (or (getf r :text) "")))
                                     (subseq t* 0 (min 400 (length t*))))))
            "iters" (getf r :iters)
            ;; provenance: a verdict from a flash model and one from a frontier
            ;; model do not carry the same weight, and the ledger should say which
            "model" (getf r :model))))

;;; ----------------------- the findings ledger ------------------------
;;; A verdict that dies with the run is not a durable claim, it is a
;;; receipt. Findings are appended here instead, one JSON object per line.
;;;
;;; Deliberately NOT operandi-notes.md: that file is read into the context
;;; of every single run, so anything written there is a tax on every future
;;; run forever. Notes are for the handful of things an agent should always
;;; know; the ledger is for everything that was ever settled, and is read
;;; only when asked. JSONL because appending must never rewrite the file —
;;; concurrent runs share it.
;;;
;;; Every record carries provenance (when, which project, which commit),
;;; because a claim about code is only true as of a revision. A finding
;;; whose commit no longer matches HEAD is a lead, not a fact, and the
;;; Findings tool says so rather than letting it read as current.

(defparameter *findings-file*
  (merge-pathnames ".operandi/findings.jsonl" (user-homedir-pathname))
  "Append-only ledger of settled claims, one JSON object per line.")

(defun %git-head ()
  "Short HEAD of the working directory's repo, or NIL outside one."
  (handler-case
      (let ((out (uiop:run-program (list "git" "rev-parse" "--short" "HEAD")
                                   :output :string :error-output nil
                                   :ignore-error-status t)))
        (let ((s (string-trim '(#\Space #\Newline #\Return) (or out ""))))
          (and (plusp (length s)) s)))
    (error () nil)))

(defun %now-iso ()
  (multiple-value-bind (sec min hr day mon yr) (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D" yr mon day hr min sec)))

(defun record-findings (records project commit)
  "Append RECORDS (the verdict objects) to the ledger with provenance.
   Best-effort: a ledger write must never take down the run that produced
   the finding."
  (handler-case
      (progn
        (ensure-directories-exist *findings-file*)
        (with-open-file (out *findings-file* :direction :output
                                             :if-exists :append
                                             :if-does-not-exist :create
                                             :external-format :utf-8)
          (let ((ts (%now-iso)))
            (dolist (r records)
              (let ((rec (llm:ht "ts" ts "project" project)))
                (when commit (setf (gethash "commit" rec) commit))
                (maphash (lambda (k v)
                           (unless (string= k "n")      ; batch-local index
                             (setf (gethash k rec) v)))
                         r)
                (write-line (jzon:stringify rec) out)))))
        t)
    (error () nil)))

(defun read-findings ()
  "Every recorded finding, newest last. Unparseable lines are skipped —
   a corrupt line must not hide the rest of the ledger."
  (handler-case
      (when (probe-file *findings-file*)
        (with-open-file (in *findings-file* :external-format :utf-8)
          (loop for line = (read-line in nil)
                while line
                for rec = (handler-case (jzon:parse line) (error () nil))
                when (hash-table-p rec) collect rec)))
    (error () nil)))

(defun finding-matches-p (rec needle)
  "Case-insensitive substring match over the fields worth searching."
  (or (null needle)
      (zerop (length needle))
      (some (lambda (k)
              (let ((v (gethash k rec)))
                (and (stringp v) (search needle v :test #'char-equal))))
            '("hypothesis" "evidence" "verdict" "project"))))

(tools:define-tool "Findings"
    (:description "Search what has ALREADY been settled, before you go and
settle it again. Every verdict from a previous Investigate is recorded here
with its evidence and the commit it was true at.

Check this first when a question sounds like one that may have been asked
before — re-deriving a claim someone already proved costs a whole run and
usually reaches a worse answer, because the original had the evidence in
front of it.

Treat a finding whose commit no longer matches HEAD as a LEAD, not a fact:
the code moved under it. The tool marks those."
     :schema (llm:ht
              "type" "object"
              "properties"
              (llm:ht
               "query" (llm:ht "type" "string"
                               "description" "Substring to look for in the hypothesis, evidence, verdict or project. Omit to list the most recent findings.")
               "limit" (llm:ht "type" "integer"
                               "description" "Most recent matches to return (default 10)."))
              "required" (vector)))
  (let* ((needle (let ((q (gethash "query" args))) (and (stringp q) q)))
         (limit (let ((l (gethash "limit" args))) (if (integerp l) (max 1 (min l 50)) 10)))
         (head (%git-head))
         (all (read-findings))
         (hits (remove-if-not (lambda (r) (finding-matches-p r needle)) all))
         (recent (last hits limit)))
    (if (null recent)
        (if needle
            (format nil "No finding recorded for ~S. Nothing has settled this yet." needle)
            "The findings ledger is empty.")
        (with-output-to-string (s)
          (format s "~D finding~:P~@[ matching ~S~] (newest last):~%" (length recent) needle)
          (dolist (r recent)
            (let* ((c (gethash "commit" r))
                   (stale (and c head (not (string= c head))))
                   ;; One preformatted string, not a ~@[ clause holding two
                   ;; ~A: a false ~@[ consumes exactly one argument, so a
                   ;; two-argument clause silently shifts every later
                   ;; argument by one when the condition is nil.
                   (note (if stale
                             (format nil "  (recorded at ~A — HEAD is now ~A, so treat this as a LEAD, not a fact)"
                                     c head)
                             "")))
              (format s "~%~A  [~A/~A]~A~%  claim: ~A~%  evidence: ~A~%"
                      (gethash "ts" r)
                      (gethash "verdict" r)
                      (or (gethash "confidence" r) "?")
                      note
                      (gethash "hypothesis" r)
                      (let ((e (or (gethash "evidence" r) "")))
                        (if (> (length e) 500) (concatenate 'string (subseq e 0 500) " …") e)))))))))

(defun run-investigate (hypotheses context tool-names depth &optional model)
  "Settle HYPOTHESES in parallel, batched at *FAN-MAX*. Returns a JSON
   array of verdict records plus a one-line cost footer. MODEL, when given,
   overrides *WORKER-MODEL* for this batch's workers."
  (let ((results (make-array (length hypotheses) :initial-element nil))
        ;; resolved in the orchestrator's thread, then handed to each worker:
        ;; a new thread sees the global, not this thread's dynamic bindings
        (wm (or model *worker-model*)))
    (loop for start from 0 below (length hypotheses) by *fan-max*
          for end = (min (length hypotheses) (+ start *fan-max*))
          do (let ((threads
                     (loop for i from start below end
                           collect (let ((idx i) (h (nth i hypotheses)))
                                     (bt:make-thread
                                      (lambda ()
                                        (let ((*worker-model* wm))
                                          (setf (aref results idx)
                                                (run-one-investigation h context tool-names depth))))
                                      :name (format nil "operandi-investigate-~D" i))))))
               (dolist (th threads) (ignore-errors (bt:join-thread th)))))
    (let* ((batch (roll-up-usage! results))
           (records (loop for r across results
                          for i from 1
                          when r collect (verdict-record r i)))
           (logged (record-findings records (namestring (uiop:getcwd)) (%git-head))))
      (format nil "~A~%[investigate: ~D hypotheses on ~A, ~A~A]"
              (jzon:stringify (coerce records 'vector) :pretty t)
              (length records)
              (or wm "the orchestrator's model")
              (llm:usage-summary batch)
              (if logged "; recorded to the findings ledger" "")))))

(tools:define-tool "Investigate"
    (:description "Settle several INDEPENDENT hypotheses in parallel and get
back structured VERDICTS — not essays.

Each hypothesis goes to its own fresh-context worker that gathers evidence
and reports by calling the Verdict tool. You get back one JSON record per
hypothesis: verdict (confirmed / refuted / undetermined), confidence, and
the concrete evidence. The workers' reasoning is discarded — you read
verdicts, not transcripts.

Use this when you have a question you can split into claims that are
separately checkable: which of these N explanations is the real cause,
does this invariant hold in each of these N places, which of these N
approaches actually works. State each hypothesis so that evidence could
in principle REFUTE it — 'X is the cause of Y', not 'look into X'.

Pass CONTEXT for background every worker needs, so none of them has to
rediscover it. Use Fan instead when you want work done rather than
questions answered."
     :schema (llm:ht
              "type" "object"
              "properties"
              (llm:ht
               "hypotheses" (llm:ht "type" "array"
                                    "items" (llm:ht "type" "string")
                                    "description" "Falsifiable claims, one per worker. Each must stand alone.")
               "context" (llm:ht "type" "string"
                                 "description" "Background every worker should be given as established — what you already know, so none of them re-derives it.")
               "tools" (llm:ht "type" "string"
                               "description" "Comma-separated tool names for the workers (default: all).")
               "model" (llm:ht "type" "string"
                               "description" "Model for THIS batch's workers — an OpenRouter vendor/name slug, or \"llama\". Omit to use the configured worker model. Escalate only the hypotheses that a cheap worker left undetermined."))
              "required" (vector "hypotheses")))
  (cond
    ((>= *subagent-depth* *subagent-max-depth*)
     (format nil "Investigate refused: subagent depth ~A >= max ~A"
             *subagent-depth* *subagent-max-depth*))
    (t
     (let* ((raw (gethash "hypotheses" args))
            (hypotheses (remove-if-not #'stringp
                                       (coerce (if (vectorp raw) raw (vector)) 'list)))
            (context (gethash "context" args))
            ;; the worker cannot report without Verdict, so it is always present
            (tool-names (adjoin "Verdict" (parse-tool-names (gethash "tools" args))
                                :test #'string=)))
       (multiple-value-bind (model ok) (parse-worker-model (gethash "model" args))
         (cond
           ((null hypotheses)
            "Investigate: give at least one hypothesis (an array of strings).")
           ((not ok)
            (format nil "Investigate: ~S is not a model spec — give a vendor/name slug or \"llama\"."
                    (gethash "model" args)))
           (t (run-investigate hypotheses context tool-names (1+ *subagent-depth*) model))))))))

(tools:define-tool "Task"
    (:description "Delegate a focused subtask to a sub-operandi with its
own fresh context. The subagent's tool calls and intermediate results
do NOT pollute your conversation — you see only its final answer.

Use Task for: multi-step exploration, focused codebase reads, anything
where you'd otherwise issue 5+ tool calls. Pass a clear DESCRIPTION
of what you want done; pass TOOLS as a string (comma-separated) to
restrict what the subagent can use.

Costs ~the time of a small operandi run. Don't use for trivial single
queries — call the tool yourself."
     :schema (llm:ht
              "type" "object"
              "properties"
              (llm:ht
               "description" (llm:ht "type" "string"
                                      "description" "What the subagent should do — be specific.")
               "tools" (llm:ht "type" "string"
                                "description" "Comma-separated tool names (default: all). E.g. 'Eval,Read,Glob'."))
              "required" (vector "description")))
  (cond
    ((>= *subagent-depth* *subagent-max-depth*)
     (format nil "Task refused: subagent depth ~A >= max ~A"
             *subagent-depth* *subagent-max-depth*))
    (t
     (let* ((desc (gethash "description" args))
            (tool-names (parse-tool-names (gethash "tools" args)))
            (r (run-one-subagent desc tool-names (1+ *subagent-depth*))))
       (when eng:*subagent-usage*
         (llm:usage-incf eng:*subagent-usage* (getf r :usage)))
       (format nil "(subagent ran ~A iterations, ~A)~%~A"
               (getf r :iters) (llm:usage-summary (getf r :usage)) (getf r :text))))))

(tools:define-tool "Fan"
    (:description "Run several INDEPENDENT subtasks IN PARALLEL, each in its
own fresh-context sub-operandi, and get back all their final answers at once.
Use this when you have N pieces that don't depend on each other — survey N
files, try N approaches, research N questions — it's far faster than calling
Task N times. Each subagent is isolated: they can't see each other or you.
Pass TASKS as an array of specific instruction strings; optionally restrict
TOOLS (comma-separated) for all of them."
     :schema (llm:ht
              "type" "object"
              "properties"
              (llm:ht
               "tasks" (llm:ht "type" "array"
                               "items" (llm:ht "type" "string")
                               "description" "Independent subtask descriptions — be specific.")
               "tools" (llm:ht "type" "string"
                                "description" "Comma-separated tool names for every subagent (default: all)."))
              "required" (vector "tasks")))
  (cond
    ((>= *subagent-depth* *subagent-max-depth*)
     (format nil "Fan refused: subagent depth ~A >= max ~A"
             *subagent-depth* *subagent-max-depth*))
    (t
     (let* ((raw (gethash "tasks" args))
            (tasks (remove-if-not #'stringp
                                  (cond ((vectorp raw) (coerce raw 'list))
                                        ((listp raw) raw)
                                        (t nil))))
            (tool-names (parse-tool-names (gethash "tools" args))))
       (cond
         ((null tasks) "Fan: TASKS must be a non-empty array of strings")
         (t (run-fan tasks tool-names (1+ *subagent-depth*))))))))

;;; ------------------ persistent, resumable subagents ------------------
;;; Task/Fan are fire-and-forget. Spawn keeps a subagent's conversation
;;; alive under a handle; SendMessage resumes it (via ENG:RUN's :history),
;;; so a subtask can be an ongoing dialogue rather than one shot. The
;;; registry (ENG:*SUBAGENTS*) is per-run, so handles live for the run.

(defstruct (sub-agent (:conc-name sa-))
  handle messages tools depth (turns 1))

(defun register-subagent (messages tools depth)
  "Store a new subagent conversation under a fresh handle; return the handle."
  (let* ((handle (format nil "agent-~D" (1+ (hash-table-count eng:*subagents*))))
         (sa (make-sub-agent :handle handle :messages messages
                             :tools tools :depth depth)))
    (setf (gethash handle eng:*subagents*) sa)
    handle))

(tools:define-tool "Spawn"
    (:description "Start a PERSISTENT subagent you can talk to again. Like Task
it runs DESCRIPTION in a fresh-context sub-operandi and returns the answer — but
it stays alive: the reply begins with a HANDLE (e.g. 'agent-1') you pass to
SendMessage to continue that SAME conversation later (it remembers everything it
did and said). Use when a subtask needs follow-ups or is an ongoing dialogue;
use Task/Fan for one-shot work. TOOLS optionally restricts its toolset."
     :schema (llm:ht
              "type" "object"
              "properties"
              (llm:ht
               "description" (llm:ht "type" "string"
                                      "description" "The subagent's first task — be specific.")
               "tools" (llm:ht "type" "string"
                                "description" "Comma-separated tool names (default: all)."))
              "required" (vector "description")))
  (cond
    ((>= *subagent-depth* *subagent-max-depth*)
     (format nil "Spawn refused: subagent depth ~A >= max ~A"
             *subagent-depth* *subagent-max-depth*))
    ((null eng:*subagents*) "Spawn: no active run (registry unavailable)")
    (t
     (let* ((desc (gethash "description" args))
            (tool-names (parse-tool-names (gethash "tools" args)))
            (depth (1+ *subagent-depth*))
            (r (run-one-subagent desc tool-names depth)))
       (when eng:*subagent-usage*
         (llm:usage-incf eng:*subagent-usage* (getf r :usage)))
       (let ((handle (register-subagent (getf r :messages) tool-names depth)))
         (format nil "handle: ~A (~A iters, ~A)~%~A"
                 handle (getf r :iters) (llm:usage-summary (getf r :usage))
                 (getf r :text)))))))

(tools:define-tool "SendMessage"
    (:description "Continue a PERSISTENT subagent created by Spawn — it remembers
its whole prior conversation. Pass its HANDLE and your MESSAGE; get its reply.
Fails if the handle is unknown (its live handles are listed in the error)."
     :schema (llm:ht
              "type" "object"
              "properties"
              (llm:ht
               "handle"  (llm:ht "type" "string"
                                  "description" "A handle from a previous Spawn, e.g. 'agent-1'.")
               "message" (llm:ht "type" "string"
                                  "description" "What to say to the subagent next."))
              "required" (vector "handle" "message")))
  (let* ((handle (gethash "handle" args))
         (msg (gethash "message" args))
         (sa (and (stringp handle) eng:*subagents*
                  (gethash handle eng:*subagents*))))
    (cond
      ((null sa)
       (format nil "SendMessage: no such handle ~S. Live handles: ~A"
               handle
               (if (and eng:*subagents* (plusp (hash-table-count eng:*subagents*)))
                   (format nil "~{~A~^, ~}"
                           (loop for k being the hash-keys of eng:*subagents* collect k))
                   "(none — Spawn one first)")))
      ((not (stringp msg)) "SendMessage: message must be a string")
      (t
       (let* ((history (append (sa-messages sa)
                               (list (llm:ht "role" "user" "content" msg))))
              (r (run-one-subagent nil (sa-tools sa) (sa-depth sa) history)))
         (setf (sa-messages sa) (getf r :messages))
         (incf (sa-turns sa))
         (when eng:*subagent-usage*
           (llm:usage-incf eng:*subagent-usage* (getf r :usage)))
         (format nil "(~A, turn ~A, ~A)~%~A"
                 handle (sa-turns sa) (llm:usage-summary (getf r :usage))
                 (getf r :text)))))))
