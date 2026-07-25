;;; bin/operandi.lisp
;;;
;;; CLI for standalone operandi.
;;;
;;; Usage:
;;;   sbcl --non-interactive --load bin/operandi.lisp -- "your task here"
;;;   sbcl --non-interactive --load bin/operandi.lisp -- shell
;;;   sbcl --non-interactive --load bin/operandi.lisp -- --openrouter [MODEL] "task"
;;;
;;; The single-task form runs the agent loop once on the given prompt
;;; and prints the final answer. The shell form is an interactive REPL:
;;; each input line becomes a fresh agent run; type 'exit' to quit.
;;; Conversation history is NOT carried across shell prompts — each task
;;; is isolated.
;;;
;;; This entry point loads ONLY operandi and its own store, so the Eval
;;; tool reaches operandi.store plus the CL standard library. A host
;;; application that wants Eval to reach richer domain packages writes
;;; its own loader: quickload :operandi, load the domain systems, then
;;; call (operandi.engine:run ...).

(require :asdf)
(unless (find-package :ql)
  (let ((init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file init) (load init))))

;; Load the library — UNLESS it's already present, e.g. we were launched from a
;; dumped core (bin/operandi) that baked it in. Skipping the quickload there is
;; what turns a ~16s cold start into ~20ms. Prefer Quicklisp (resolves deps);
;; fall back to a bare ASDF load-system if the project is registered but ql is
;; absent.
(unless (find-package :operandi.engine)
  (handler-case
      (funcall (read-from-string "ql:quickload") :operandi :silent t)
    (error ()
      (asdf:load-system :operandi))))

;; Open operandi's own store so the tool-call log + Eval have *db* live
;; without manual setup. FIND-SYMBOL so the reader needn't intern the
;; package at read-time.
(funcall (find-symbol "OPEN-STORE" "OPERANDI.STORE"))

(defpackage #:operandi-cli
  (:use #:cl)
  (:local-nicknames (#:eng #:operandi.engine)
                    (#:llm #:operandi.llm)
                    (#:tools #:operandi.tools)))
(in-package #:operandi-cli)

(defun %csv (s) (and s (mapcar (lambda (x) (string-trim " " x)) (uiop:split-string s :separator ","))))
(defun %tools-allow (csv) "The tool set for --tools: exactly the named tools." (%csv csv))
(defun %tools-deny (csv)  "The tool set for --no-tools: the defaults minus the named tools."
  (let ((deny (%csv csv)))
    (remove-if (lambda (n) (member n deny :test #'string-equal)) (tools:default-tools))))

(defun display-run (prompt &optional (tool-names (tools:default-tools)))
  "Run PROMPT streaming tokens live to stdout, then print metrics. The
   final answer is reprinted only if it differs from what already streamed
   (a synthetic '[max-iterations…]' result, or blocking mode).  TOOL-NAMES is the
   available-tools allow-list handed to the agent."
  (let* ((buf (make-string-output-stream))
         (eng:*on-token* (lambda (s) (write-string s buf) (write-string s) (force-output))))
    (multiple-value-bind (text history iters usage) (eng:run prompt :tool-names tool-names)
      (declare (ignore history))
      (let ((streamed (string-right-trim '(#\Space #\Newline) (get-output-stream-string buf))))
        (unless (string= (string-right-trim '(#\Space #\Newline) (or text "")) streamed)
          (format t "~&~A" text)))
      (format t "~&~%[~A iters, ~A]~%" iters (llm:usage-summary usage)))))

(defun run-once (prompt &optional (tool-names (tools:default-tools)))
  (handler-case (display-run prompt tool-names)
    (#+sbcl sb-sys:interactive-interrupt #-sbcl error ()
      (format t "~&interrupted.~%")
      (sb-ext:exit :code 130))))

(defun run-shell (&optional resume)
  "Launch the interactive TUI — the enhanced inline REPL with live
   streaming, tool rendering, multi-turn memory, and slash commands.
   RESUME (a session id or :LATEST) continues a saved session."
  (funcall (find-symbol "REPL" "OPERANDI.TUI") :resume resume))

(defun run-resumed-task (resume prompt)
  "Resume a saved session and run PROMPT as one more turn, non-interactively."
  (handler-case
      (funcall (find-symbol "REPL" "OPERANDI.TUI") :resume resume :once prompt :greet nil)
    (#+sbcl sb-sys:interactive-interrupt #-sbcl error ()
      (format t "~&interrupted.~%")
      (sb-ext:exit :code 130))))

(defun session-id-like (s)
  "True if S looks like a session id (YYYYMMDD-HHMMSS), so `--resume <id>` can
   tell an id apart from a following command/task."
  (and (stringp s) (= (length s) 15) (char= (char s 8) #\-)
       (every #'digit-char-p (remove #\- s))))

(let* ((raw (remove "--" (uiop:command-line-arguments) :test #'string=))
       (resume nil)
       (tool-names (tools:default-tools))     ; available-tools allow-list (default = all)
       ;; Strip leading flags (--openrouter [model], --resume [id], --tools/--no-tools) in any order.
       (args (let ((acc raw))
               (loop
                 (cond
                   ((and acc (string= (first acc) "--openrouter"))
                    (let* ((tail (rest acc)) (next (first tail))
                           ;; OpenRouter model IDs look like 'vendor/model' — a
                           ;; single slashed token. A spaced arg is the task.
                           (is-model (and next (find #\/ next) (not (find #\Space next)))))
                      (cond (is-model (llm:use-openrouter :model next) (setf acc (rest tail)))
                            (t        (llm:use-openrouter)             (setf acc tail)))))
                   ((and acc (string= (first acc) "--resume"))
                    (let* ((tail (rest acc)) (next (first tail)))
                      (cond ((session-id-like next) (setf resume next    acc (rest tail)))
                            (t                       (setf resume :latest acc tail)))))
                   ;; --tools A,B,C  -> ONLY those tools; --no-tools X,Y -> defaults minus X,Y.
                   ((and acc (string= (first acc) "--tools"))
                    (setf tool-names (%tools-allow (second acc)) acc (cddr acc)))
                   ((and acc (string= (first acc) "--no-tools"))
                    (setf tool-names (%tools-deny (second acc)) acc (cddr acc)))
                   (t (return))))
               acc))
       (cmd (first args)))
  (cond
    ;; --resume alone or with an explicit tui command → resumed interactive TUI
    ((and resume (or (null cmd) (member cmd '("tui" "shell" "repl") :test #'string-equal)))
     (run-shell resume))
    ;; --resume + a task → continue that session with one more (non-interactive) turn
    (resume (run-resumed-task resume (format nil "~{~A~^ ~}" args)))
    ((null cmd)
     (format t "~&usage:~%")
     (format t "  operandi.lisp -- \"task description\"~%")
     (format t "  operandi.lisp -- tui              (interactive REPL: streaming, tools, /commands)~%")
     (format t "  operandi.lisp -- --resume [ID] tui  (resume a saved session; latest if no ID)~%")
     (format t "  operandi.lisp -- --openrouter [MODEL] \"task\"~%")
     (format t "  operandi.lisp -- --tools Read,Write,Edit,Bash,Grep \"task\"   (allow-list)~%")
     (format t "  operandi.lisp -- --no-tools Fan,Task,Spawn \"task\"           (defaults minus these)~%")
     (format t "~%env: OPERANDI_MAX_TOKENS (per-turn output cap, default 16384),~%")
     (format t "     OPERANDI_CONTEXT_BUDGET (compaction threshold), OPERANDI_MAX_ITERS.~%")
     (format t "~%--openrouter reads token from ~~/.operandi/openrouter.token.~%")
     (format t "Sessions are saved under ~~/.operandi/sessions/; --resume continues one.~%")
     (format t "Default backend is a local llama.cpp on http://127.0.0.1:8081.~%"))
    ((member cmd '("tui" "shell" "repl") :test #'string-equal) (run-shell))
    (t (run-once (format nil "~{~A~^ ~}" args) tool-names))))

;; Exit cleanly once the command is done. Without this, an invocation that
;; lacks --non-interactive (e.g. `sbcl --noinform --load bin/operandi.lisp`)
;; drops into SBCL's own REPL after the TUI quits — forcing an extra Ctrl-D.
;; (`bin/operandi` and the MCP already pass --non-interactive; this is harmless
;; there and makes the bare --load form behave the same.)
(sb-ext:exit :code 0)
