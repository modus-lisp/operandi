;;; inspect/store-concurrency-test.lisp
;;;
;;; Oracle for thread-safe store access — the foundation that lets
;;; parallel subagents (Fan) log their tool calls instead of running
;;; blind. Many writer threads hammer the tool-call log at once while
;;; reader threads count concurrently. The log hook swallows its own
;;; errors, so a lost row is the tell: if the lock weren't there, some
;;; concurrent writes would fail and the final count would be short.
;;;
;;;   sbcl --non-interactive --load inspect/store-concurrency-test.lisp  (0/1)

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.store-concurrency-test
  (:use #:cl)
  (:local-nicknames (#:store #:operandi.store)
                    (#:hooks #:operandi.hooks)
                    (#:llm   #:operandi.llm)
                    (#:bt    #:bordeaux-threads)))
(in-package #:operandi.store-concurrency-test)

(defvar *pass* 0) (defvar *fail* 0)

(defmacro check (name &body body)
  `(handler-case
       (if (progn ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A~%" ,name)))
     (serious-condition (e)
       (incf *fail*) (format t "~&  FAIL ~A (~A)~%" ,name (type-of e)))))

(defparameter *writers* 8)
(defparameter *per-writer* 150)
(defparameter *readers* 3)

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&operandi store concurrency oracle~%")
  (store:open-store (merge-pathnames "operandi-store-conc-test.db"
                                     (uiop:temporary-directory)))
  (store:exec "DELETE FROM agent_tool_calls")

  (let ((reader-errs 0))
    (let ((writers
            (loop for w below *writers*
                  collect (bt:make-thread
                           (let ((wid w))
                             (lambda ()
                               (let ((hooks:*current-run-id* (format nil "run-~D" wid)))
                                 (declare (special hooks:*current-run-id*))
                                 (dotimes (i *per-writer*)
                                   ;; the REAL default logging hook
                                   (hooks:log-tool-call-to-sqlite
                                    "Test" (llm:ht "w" wid "i" i) "ok" nil 3)))))
                           :name (format nil "writer-~D" w))))
          (readers
            (loop repeat *readers*
                  collect (bt:make-thread
                           (lambda ()
                             (dotimes (i 300)
                               (handler-case
                                   (store:select-single
                                    "SELECT count(*) FROM agent_tool_calls")
                                 (error () (incf reader-errs)))))
                           :name "reader"))))
      (dolist (th (append writers readers)) (bt:join-thread th)))

    (check "every concurrent write landed (none lost to a race)"
      (= (store:select-single "SELECT count(*) FROM agent_tool_calls")
         (* *writers* *per-writer*)))

    (check "each writer's rows are all present under its own run_id"
      (loop for w below *writers*
            always (= *per-writer*
                      (store:select-single
                       "SELECT count(*) FROM agent_tool_calls WHERE run_id = ?"
                       (format nil "run-~D" w)))))

    (check "concurrent readers never errored on the shared connection"
      (zerop reader-errs))

    (check "with-tx is atomic under concurrency"
      ;; interleave transactional multi-row inserts from several threads;
      ;; each tx adds 2 rows, so the total must be exactly 2*threads.
      (progn
        (store:exec "DELETE FROM agent_tool_calls")
        (let ((ts (loop repeat 6
                        collect (bt:make-thread
                                 (lambda ()
                                   (dotimes (i 20)
                                     (store:with-tx
                                       (store:exec "INSERT INTO agent_tool_calls (tool_name, at) VALUES ('a', 0)")
                                       (store:exec "INSERT INTO agent_tool_calls (tool_name, at) VALUES ('b', 0)"))))))))
          (dolist (th ts) (bt:join-thread th)))
        (= (store:select-single "SELECT count(*) FROM agent_tool_calls")
           (* 6 20 2)))))

  (store:close-store)
  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
