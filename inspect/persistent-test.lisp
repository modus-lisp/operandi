;;; inspect/persistent-test.lisp
;;;
;;; Oracle for persistent, resumable subagents (Spawn + SendMessage).
;;; eng:run is stubbed with a CONTEXT-AWARE mock: it "recalls" a secret
;;; only if that secret appears somewhere in the conversation it's given.
;;; So a SendMessage reply that recalls a secret introduced at Spawn time
;;; proves the prior conversation was actually resumed — and an agent that
;;; never saw the secret proves it isn't just always echoing.
;;;
;;;   sbcl --non-interactive --load inspect/persistent-test.lisp   (0/1)

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.persistent-test
  (:use #:cl)
  (:local-nicknames (#:tools #:operandi.tools)
                    (#:sub   #:operandi.subagent)
                    (#:eng   #:operandi.engine)
                    (#:llm   #:operandi.llm)))
(in-package #:operandi.persistent-test)

(defvar *pass* 0) (defvar *fail* 0)

(defmacro check (name &body body)
  `(handler-case
       (if (progn ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A~%" ,name)))
     (serious-condition (e)
       (incf *fail*) (format t "~&  FAIL ~A (~A)~%" ,name (type-of e)))))

;; Context-aware stub: reply "recalled: ZEBRA" iff ZEBRA is anywhere in the
;; conversation it receives (fresh prompt or resumed :history), else deny.
;; Returns (text messages iters usage) like the real run.
(setf (symbol-function 'operandi.engine:run)
      (lambda (prompt &rest keys)
        (let* ((history (getf keys :history))
               (msgs (or history
                         (list (llm:ht "role" "system" "content" "s")
                               (llm:ht "role" "user" "content" (or prompt "")))))
               (secret (loop for m in msgs
                             for c = (gethash "content" m)
                             when (and (stringp c) (search "ZEBRA" c)) return "ZEBRA"))
               (reply (if secret (format nil "recalled: ~A" secret) "no memory of that")))
          (values reply
                  (append msgs (list (llm:ht "role" "assistant" "content" reply)))
                  1 (llm:make-usage :calls 1 :cost-usd 0.001d0)))))

(defmacro with-run (&body body)
  "Bind a fresh per-run registry + usage accumulator (RUN would normally)."
  `(let ((eng:*subagents* (make-hash-table :test 'equal))
         (eng:*subagent-usage* (llm:make-usage)))
     ,@body))

(defun spawn (desc) (tools:invoke-tool "Spawn" (llm:ht "description" desc)))
(defun send (h m)   (tools:invoke-tool "SendMessage" (llm:ht "handle" h "message" m)))
(defun handle-of (spawn-reply)
  (let ((p (search "agent-" spawn-reply)))
    (and p (subseq spawn-reply p (position #\Space spawn-reply :start p)))))

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&operandi persistent-subagent (Spawn + SendMessage) oracle~%")

  (check "Spawn returns a handle"
    (with-run (search "handle: agent-1" (spawn "The password is ZEBRA. Acknowledge."))))

  (check "SendMessage resumes the conversation (recalls the spawn secret)"
    (with-run
      (let ((h (handle-of (spawn "The password is ZEBRA. Acknowledge."))))
        (search "ZEBRA" (send h "What was the password?")))))

  (check "isolation: an agent that never saw the secret can't recall it"
    (with-run
      (let ((h (handle-of (spawn "Say hello."))))
        (not (search "ZEBRA" (send h "What was the password?"))))))

  (check "two Spawns get distinct handles"
    (with-run
      (let ((h1 (handle-of (spawn "a"))) (h2 (handle-of (spawn "b"))))
        (and (string= h1 "agent-1") (string= h2 "agent-2")))))

  (check "SendMessage to an unknown handle errors and lists live handles"
    (with-run
      (spawn "x")
      (let ((r (send "agent-99" "hi")))
        (and (search "no such handle" r) (search "agent-1" r)))))

  (check "turn counter advances across messages"
    (with-run
      (let ((h (handle-of (spawn "start"))))     ; turn 1
        (send h "one")                            ; turn 2
        (search "turn 3" (send h "two")))))       ; turn 3

  (check "Spawn + SendMessage usage rolls into the parent accumulator"
    (with-run
      (let ((h (handle-of (spawn "hi"))))         ; 0.001
        (send h "more")                            ; +0.001
        (> (llm:usage-cost-usd eng:*subagent-usage*) 0.0015))))

  (check "Spawn refuses past max depth"
    (with-run
      (let ((sub:*subagent-depth* sub:*subagent-max-depth*))
        (search "refused" (spawn "x")))))

  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
