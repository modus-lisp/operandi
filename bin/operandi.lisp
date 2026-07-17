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
                    (#:llm #:operandi.llm)))
(in-package #:operandi-cli)

(defun display-run (prompt)
  "Run PROMPT streaming tokens live to stdout, then print metrics. The
   final answer is reprinted only if it differs from what already streamed
   (a synthetic '[max-iterations…]' result, or blocking mode)."
  (let* ((buf (make-string-output-stream))
         (eng:*on-token* (lambda (s) (write-string s buf) (write-string s) (force-output))))
    (multiple-value-bind (text history iters usage) (eng:run prompt)
      (declare (ignore history))
      (let ((streamed (string-right-trim '(#\Space #\Newline) (get-output-stream-string buf))))
        (unless (string= (string-right-trim '(#\Space #\Newline) (or text "")) streamed)
          (format t "~&~A" text)))
      (format t "~&~%[~A iters, ~A]~%" iters (llm:usage-summary usage)))))

(defun run-once (prompt)
  (handler-case (display-run prompt)
    (#+sbcl sb-sys:interactive-interrupt #-sbcl error ()
      (format t "~&interrupted.~%")
      (sb-ext:exit :code 130))))

(defun run-shell ()
  "Launch the interactive TUI — the enhanced inline REPL with live
   streaming, tool rendering, multi-turn memory, and slash commands."
  (funcall (find-symbol "REPL" "OPERANDI.TUI")))

(let* ((raw (remove "--" (uiop:command-line-arguments) :test #'string=))
       ;; Strip --openrouter [model] flag if present and switch backend.
       (args (let ((acc raw))
               (when (and acc (string= (first acc) "--openrouter"))
                 (let* ((tail (rest acc))
                        (next (first tail))
                        ;; OpenRouter model IDs look like 'vendor/model' —
                        ;; single token, slash inside, no spaces. If the
                        ;; next arg has spaces it's the task, not a model.
                        (is-model (and next (find #\/ next)
                                       (not (find #\Space next)))))
                   (cond
                     (is-model (llm:use-openrouter :model next) (setf acc (rest tail)))
                     (t        (llm:use-openrouter)             (setf acc tail)))))
               acc))
       (cmd (first args)))
  (cond
    ((null cmd)
     (format t "~&usage:~%")
     (format t "  operandi.lisp -- \"task description\"~%")
     (format t "  operandi.lisp -- tui        (interactive REPL: streaming, tools, /commands)~%")
     (format t "  operandi.lisp -- --openrouter [MODEL] \"task\"~%")
     (format t "~%--openrouter reads token from ~~/.operandi/openrouter.token.~%")
     (format t "Default backend is a local llama.cpp on http://127.0.0.1:8081.~%"))
    ((member cmd '("tui" "shell" "repl") :test #'string-equal) (run-shell))
    (t (run-once (format nil "~{~A~^ ~}" args)))))
