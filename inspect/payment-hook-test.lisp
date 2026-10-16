;;; inspect/payment-hook-test.lisp
;;;
;;; Oracle for the 402 -> pay -> retry path (no network, no money): a stub
;;; hook stands in for "invoice the operator and wait". The property that
;;; matters is SINGLE-FLIGHT: workers that hit an empty account at the same
;;; time must cause ONE invoice, not one each.
;;;
;;;   sbcl --non-interactive --load inspect/payment-hook-test.lisp   (0/1)

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.payment-hook-test
  (:use #:cl) (:local-nicknames (#:eng #:operandi.engine) (#:bt #:bordeaux-threads)))
(in-package #:operandi.payment-hook-test)

(defvar *pass* 0) (defvar *fail* 0)
(defmacro check (name &body body)
  `(handler-case
       (if (progn ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A~%" ,name)))
     (serious-condition (e) (incf *fail*) (format t "~&  FAIL ~A (~A)~%" ,name e))))

(defun run ()
  (format t "~&operandi payment-required oracle~%")
  (check "402 / insufficient credit is recognised; other errors are not"
    (and (eng:payment-required-p "[provider error] Insufficient credits (402)")
         (eng:payment-required-p "[provider error] insufficient balance")
         (not (eng:payment-required-p "[provider error] model not found (404)"))
         (not (eng:payment-required-p "cannot reach https://x (TIMEOUT)"))))
  (check "no hook installed: nothing happens, no retry"
    (let ((eng:*payment-required-hook* nil)) (null (eng:handle-payment-required))))
  (check "a failing hook: no retry (and it does not signal)"
    (let ((eng:*payment-required-hook* (lambda () (error "wallet offline"))))
      (null (eng:handle-payment-required))))
  (let* ((calls 0)
         (results (make-array 8 :initial-element :unset)))
    (setf eng:*payment-required-hook* (lambda () (incf calls) (sleep 1) t))
    (unwind-protect
         (let ((threads (loop for i below 8
                              collect (let ((i i))
                                        (bt:make-thread (lambda () (setf (aref results i) (eng:handle-payment-required))))))))
           (mapc #'bt:join-thread threads)
           (check "8 workers hit the 402 at once: the hook runs ONCE (one invoice)" (= calls 1))
           (check "…and all 8 are told to retry" (every (lambda (r) (eq r t)) results)))
      (setf eng:*payment-required-hook* nil)))
  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
