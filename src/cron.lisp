;;; src/cron.lisp
;;;
;;; Scheduled-task runner that lives INSIDE a long-running Lisp image
;;; — a thread inside the paper trader (or any host process) that wakes
;;; up periodically, checks which operandi tasks are due, and fires
;;; them.
;;;
;;; Design:
;;;   * The schedule is data: `*schedule*' is a list of plists, each
;;;     describing a task (name, interval-hours, prompt, tools).
;;;   * `*last-runs*' tracks last-run-time per task. Persisted to
;;;     `*state-file*' so restarts don't re-fire everything immediately.
;;;   * `tick' is a synchronous one-shot — examines the schedule, runs
;;;     anything due, returns. Safe to call from any thread or the
;;;     paper trader's per-cycle hook.
;;;   * `start-thread' spawns a bordeaux thread that calls TICK every
;;;     `*tick-sec*' seconds.
;;;
;;; The point: don't boot a new sbcl every interval. The image is
;;; already running and has all the packages loaded.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (ql:quickload '(:bordeaux-threads :uiop) :silent t))

(defpackage #:operandi.cron
  (:use #:cl)
  (:local-nicknames (#:bt   #:bordeaux-threads)
                    (#:eng  #:operandi.engine))
  (:export #:*schedule*
           #:*reports-dir*
           #:*log-file*
           #:*state-file*
           #:*tick-sec*
           #:*running*
           #:tick
           #:run-task
           #:run-once-by-name
           #:list-schedule
           #:start-thread
           #:stop-thread))

(in-package #:operandi.cron)

(defparameter *reports-dir*
  (namestring (merge-pathnames ".operandi/reports/" (user-homedir-pathname))))
(defparameter *log-file*
  (namestring (merge-pathnames ".operandi/cron.log" (user-homedir-pathname))))
(defparameter *state-file*
  (namestring (merge-pathnames ".operandi/cron-state.lisp" (user-homedir-pathname))))
(defparameter *tick-sec*    60
  "Seconds between TICK invocations from the background thread.")

(defparameter *schedule* '()
  "List of scheduled-task plists. Each entry is
     (:name STRING :every-hours N :tools (\"Eval\" ...) :prompt STRING)
   TICK fires any task whose interval has elapsed, runs it through the
   agent, and writes the result to *REPORTS-DIR*. Empty by default — a
   host application populates it with its own tasks, e.g.:
     (push '(:name \"daily-summary\" :every-hours 24
             :tools (\"Eval\" \"Write\")
             :prompt \"Write a one-page status summary to ~/report.md\")
           operandi.cron:*schedule*)")

(defvar *last-runs* (make-hash-table :test 'equal))
(defvar *thread* nil)
(defvar *running* nil)

;;; ----------------------- state persistence -----------------------

(defun load-state ()
  (when (probe-file *state-file*)
    (handler-case
        (with-open-file (s *state-file*)
          (let ((data (with-standard-io-syntax (read s nil nil))))
            (when (listp data)
              (loop for (k v) on data by #'cddr
                    do (setf (gethash k *last-runs*) v)))))
      (error () nil))))

(defun save-state ()
  (handler-case
      (progn
       (ensure-directories-exist *state-file*)
       (with-open-file (out *state-file* :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
        (with-standard-io-syntax
          (let ((acc '()))
            (maphash (lambda (k v) (push v acc) (push k acc)) *last-runs*)
            (prin1 acc out)
            (terpri out)))))
    (error () nil)))

(defun log-line (line)
  (handler-case
      (progn
       (ensure-directories-exist *log-file*)
       (with-open-file (out *log-file* :direction :output
                                       :if-exists :append
                                       :if-does-not-exist :create)
        (multiple-value-bind (s mi h da mo y) (decode-universal-time
                                               (get-universal-time))
          (format out "~A-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D ~A~%"
                  y mo da h mi s line))))
    (error () nil)))

;;; ----------------------- task execution --------------------------

(defun task-due-p (task now)
  (let* ((name (getf task :name))
         (interval (* 3600 (getf task :every-hours)))
         (last (gethash name *last-runs* 0)))
    (>= (- now last) interval)))

(defun run-task (task)
  (let ((name (getf task :name))
        (prompt (getf task :prompt))
        (tools (getf task :tools)))
    (ensure-directories-exist *reports-dir*)
    (log-line (format nil "[~A] starting" name))
    (multiple-value-bind (text history iters)
        (handler-case
            (eng:run prompt :tool-names tools :verbose nil)
          (error (e)
            (log-line (format nil "[~A] ERROR: ~A" name e))
            (return-from run-task nil)))
      (declare (ignore history))
      (let* ((unix (- (get-universal-time) 2208988800))
             (path (format nil "~A~A-~A.md" *reports-dir* name unix)))
        (handler-case
            (with-open-file (out path :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
              (format out "# ~A (~A iterations)~%~%~A~%" name iters text))
          (error () nil))
        (log-line (format nil "[~A] done after ~A iter; ~A chars"
                          name iters (length (or text ""))))
        (setf (gethash name *last-runs*) (get-universal-time))
        (save-state)
        text))))

(defun run-once-by-name (name)
  (load-state)
  (let ((task (find name *schedule* :key (lambda (t-) (getf t- :name))
                     :test #'string=)))
    (cond
      ((null task) (format t "no such task: ~A~%" name) nil)
      (t (run-task task)))))

(defun tick ()
  "Synchronous one-shot. Examines the schedule, runs anything due.
   Safe to call from any thread."
  (load-state)
  (let ((now (get-universal-time))
        (fired 0))
    (dolist (task *schedule*)
      (when (task-due-p task now)
        (run-task task)
        (incf fired)))
    fired))

(defun list-schedule ()
  (load-state)
  (format t "~&~20A ~10A ~A~%" "task" "every" "next-fire")
  (let ((now (get-universal-time)))
    (dolist (task *schedule*)
      (let* ((name (getf task :name))
             (every (* 3600 (getf task :every-hours)))
             (last (gethash name *last-runs* 0))
             (due (+ last every))
             (delta (- due now)))
        (format t "~20A ~10A ~A~%"
                name
                (format nil "~Ah" (getf task :every-hours))
                (cond ((zerop last) "(never run)")
                      ((minusp delta) (format nil "OVERDUE by ~As" (abs delta)))
                      (t (format nil "in ~As" delta))))))))

;;; ----------------------- thread mode -----------------------------

(defun start-thread ()
  "Spawn a bordeaux thread that calls TICK every *TICK-SEC* seconds.
   Idempotent — calling twice doesn't start a second thread."
  (when (and *thread* (bt:thread-alive-p *thread*))
    (return-from start-thread *thread*))
  (setf *running* t)
  (setf *thread*
        (bt:make-thread
         (lambda ()
           (log-line "operandi-cron thread started")
           (loop while *running* do
                 (handler-case (tick)
                   (error (e) (log-line (format nil "TICK ERROR: ~A" e))))
                 (sleep *tick-sec*))
           (log-line "operandi-cron thread stopped"))
         :name "operandi-cron"))
  *thread*)

(defun stop-thread ()
  "Signal the thread to exit at its next wake. Returns once it's gone."
  (setf *running* nil)
  (when *thread*
    (handler-case (bt:join-thread *thread*)
      (error () nil))
    (setf *thread* nil)))
