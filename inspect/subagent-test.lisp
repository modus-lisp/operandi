;;; inspect/subagent-test.lisp
;;;
;;; Oracle for subagents — the single Task and the parallel Fan. eng:run
;;; is stubbed (with a sleep) so we can prove Fan actually runs subtasks
;;; concurrently, isolates failures, batches beyond *fan-max*, sums cost,
;;; and honours the depth cap — all deterministically, no network.
;;;
;;;   sbcl --non-interactive --load inspect/subagent-test.lisp   (0/1)

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.subagent-test
  (:use #:cl)
  (:local-nicknames (#:tools #:operandi.tools)
                    (#:sub   #:operandi.subagent)
                    (#:llm   #:operandi.llm)))
(in-package #:operandi.subagent-test)

(defvar *pass* 0) (defvar *fail* 0)
(defvar *stub-sleep* 0.0)   ; SETF (not LET) — child threads read the global

(defmacro check (name &body body)
  `(handler-case
       (if (progn ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A~%" ,name)))
     (serious-condition (e)
       (incf *fail*) (format t "~&  FAIL ~A (~A)~%" ,name (type-of e)))))

;; Stub the engine: each "subagent" sleeps then returns a scripted result.
;; A task containing BOOM raises, to test failure isolation.
(setf (symbol-function 'operandi.engine:run)
      (lambda (prompt &rest keys)
        (declare (ignore keys))
        (when (search "BOOM" prompt) (error "boom in subagent"))
        (when (plusp *stub-sleep*) (sleep *stub-sleep*))
        ;; position 4 is now a usage struct (was a bare cost number).
        (values (format nil "answer:~A" prompt) nil 3
                (llm:make-usage :calls 1 :prompt-tokens 100
                                :completion-tokens 50 :cost-usd 0.001d0))))

(defun fan (tasks) (tools:invoke-tool "Fan" (llm:ht "tasks" tasks)))

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&operandi subagent (Task + parallel Fan) oracle~%")

  (setf *stub-sleep* 0.0)

  (check "Task still returns a subagent result"
    (search "answer:solo" (tools:invoke-tool "Task" (llm:ht "description" "solo"))))

  (check "parse-tool-names splits and defaults"
    (and (equal '("Read" "Eval") (sub::parse-tool-names "Read, Eval"))
         (member "Task" (sub::parse-tool-names nil) :test #'string=)))

  (check "Fan returns every subtask's answer, labeled and in order"
    (let ((r (fan (vector "t1" "t2" "t3"))))
      (and (search "subagent 1" r) (search "subagent 3" r)
           (search "answer:t1" r) (search "answer:t3" r)
           (search "3 subagents" r))))

  ;; Concurrency: 4 tasks x 0.25s each. Sequential would be ~1.0s;
  ;; parallel (fan-max 8) is ~0.25s. Assert well under sequential.
  (check "Fan runs tasks concurrently"
    (progn
      (setf *stub-sleep* 0.25)
      (let* ((t0 (get-internal-real-time))
             (r (let ((sub:*fan-max* 8)) (fan (vector "a" "b" "c" "d"))))
             (secs (/ (- (get-internal-real-time) t0)
                      internal-time-units-per-second)))
        (setf *stub-sleep* 0.0)
        (and (search "answer:d" r) (< secs 0.7)))))

  (check "Fan batches beyond *fan-max* but still runs all"
    (let ((sub:*fan-max* 2))
      (let ((r (fan (vector "a" "b" "c" "d" "e"))))
        (and (search "subagent 5" r) (search "answer:e" r)))))

  (check "Fan isolates a failing subagent"
    (let ((r (fan (vector "ok1" "BOOM" "ok3"))))
      (and (search "SUBAGENT ERROR" r)
           (search "answer:ok1" r) (search "answer:ok3" r))))

  (check "Fan rejects a non-array tasks value"
    (search "array" (tools:invoke-tool "Fan" (llm:ht "tasks" "notarray"))))

  (check "Fan refuses past max depth"
    (let ((sub:*subagent-depth* sub:*subagent-max-depth*))
      (search "refused" (fan (vector "x")))))

  ;; cost propagation: Task/Fan roll each subagent's usage into the
  ;; parent run's accumulator (here we bind it directly).
  (check "Fan rolls subagent usage into the parent accumulator"
    (let ((operandi.engine:*subagent-usage* (llm:make-usage)))
      (fan (vector "a" "b" "c"))               ; 3 x 0.001 cost, 3 x 150 tok
      (and (> (llm:usage-cost-usd operandi.engine:*subagent-usage*) 0.0025)
           (= (llm:usage-prompt-tokens operandi.engine:*subagent-usage*) 300))))

  (check "Task rolls its subagent usage into the parent accumulator"
    (let ((operandi.engine:*subagent-usage* (llm:make-usage)))
      (tools:invoke-tool "Task" (llm:ht "description" "solo"))
      (and (> (llm:usage-cost-usd operandi.engine:*subagent-usage*) 0.0005)
           (= (llm:usage-completion-tokens operandi.engine:*subagent-usage*) 50))))

  ;; the Fan footer surfaces the aggregated usage
  (check "Fan footer reports aggregated usage"
    (search "tok" (fan (vector "a" "b"))))

  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
