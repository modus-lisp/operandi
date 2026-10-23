;;; inspect/trust-test.lisp
;;;
;;; Oracle for --confidential (seal every request to an attestation verified
;;; here) and --trust=LIST (the operators trusted with the cleartext).  No
;;; network: every check is a predicate over the backend switch, the flag, the
;;; trust list, or a request body.
;;;
;;; What it pins down:
;;;   - --trust names are validated; a typo is reported, not silently dropped
;;;   - maple needs tinfoil-shared needs tinfoil-inference; a name without what
;;;     it needs is refused
;;;   - an unsealed path is refused whatever --trust says
;;;   - a sealed path is used only when its operator is trusted
;;;   - with-ppq checks the claim at RUN time, and only when it is on (the
;;;     subagent passes the flag as a runtime form, which used to trip the check
;;;     even with confidentiality off)
;;;   - the resume command restores the claim and its declarations
;;;
;;;   sbcl --non-interactive --load inspect/trust-test.lisp   (exits 0/1)

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.trust-test
  (:use #:cl)
  (:local-nicknames (#:llm #:operandi.llm))
  (:export #:run))
(in-package #:operandi.trust-test)

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro check (name &body body)
  `(handler-case
       (if (progn ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A~%" ,name)))
     (serious-condition (e)
       (incf *fail*) (format t "~&  FAIL ~A (~A)~%" ,name (type-of e)))))

(defun refused-p (thunk)
  "True when THUNK signals an error."
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defun claim-refused-p (backend model trust)
  (let ((llm:*llm-backend* backend) (llm:*llm-model* model)
        (llm:*confidential* t) (llm:*trust* trust))
    (refused-p #'llm:require-confidential!)))

(defparameter *tinfoil-trust* '("tinfoil-shared" "tinfoil-inference"))

(defparameter *token*
  (format nil "~A/operandi-trust-test.token" (uiop:temporary-directory))
  "A PPQ key file for with-ppq, so each check reaches the claim logic and not the
   missing-key refusal.")

(defun run-checks ()

  (check "parse-trust-list: lower-cases, trims, and drops empty pieces"
    (equal (llm:parse-trust-list " Maple,, Tinfoil-Shared ,") '("maple" "tinfoil-shared")))
  (check "parse-trust-list: an unknown name is reported as bad"
    (equal (nth-value 1 (llm:parse-trust-list "maple,tinfoil")) '("tinfoil")))

  (check "requirements: maple needs both Tinfoil names, tinfoil-shared needs inference, inference needs nothing"
    (and (equal (llm:trust-requirements "maple") '("tinfoil-shared" "tinfoil-inference"))
         (equal (llm:trust-requirements "tinfoil-shared") '("tinfoil-inference"))
         (null (llm:trust-requirements "tinfoil-inference"))))
  (check "check-trust-list!: maple alone, or with only tinfoil-shared, is refused"
    (and (refused-p (lambda () (llm:check-trust-list! '("maple"))))
         (refused-p (lambda () (llm:check-trust-list! '("maple" "tinfoil-shared"))))
         (refused-p (lambda () (llm:check-trust-list! '("tinfoil-shared"))))))
  (check "check-trust-list!: each complete chain is accepted"
    (and (llm:check-trust-list! '("maple" "tinfoil-shared" "tinfoil-inference"))
         (llm:check-trust-list! *tinfoil-trust*)
         (llm:check-trust-list! '("tinfoil-inference"))
         (llm:check-trust-list! nil)))
  (check "startup: an incomplete --trust is refused even without --confidential"
    (let ((llm:*llm-backend* :openrouter) (llm:*confidential* nil) (llm:*trust* '("maple")))
      (refused-p #'llm:require-confidential!)))

  (check "confidential: the Tinfoil router path needs tinfoil-shared"
    (equal (llm:path-trust :tinfoil "gpt-oss-120b") "tinfoil-shared"))
  (check "confidential: allowed with the Tinfoil chain declared"
    (not (claim-refused-p :tinfoil "gpt-oss-120b" *tinfoil-trust*)))
  (check "confidential: a sealed path whose operator is not trusted is refused"
    (and (llm:sealable-p :tinfoil "gpt-oss-120b")
         (claim-refused-p :tinfoil "gpt-oss-120b" nil)
         (claim-refused-p :tinfoil "gpt-oss-120b" '("tinfoil-inference"))))
  (check "confidential: an unsealed path is refused whatever --trust says"
    (and (claim-refused-p :openrouter "x" *tinfoil-trust*)
         (claim-refused-p :llama "x" '("maple" "tinfoil-shared" "tinfoil-inference"))))
  (check "no --confidential, no seal check"
    (let ((llm:*llm-backend* :openrouter) (llm:*confidential* nil) (llm:*trust* nil))
      (not (refused-p #'llm:require-confidential!))))

  (check "with-ppq: a plaintext PPQ model under --confidential signals, whatever --trust says"
    (let ((llm:*trust* *tinfoil-trust*))
      (refused-p (lambda ()
                   (llm:with-ppq (:model "gpt-6.1-sol" :confidential t :token-file *token*)
                     nil)))))
  (check "with-ppq: the same call is accepted when --confidential is off (the subagent case)"
    (let ((llm:*trust* nil))
      (not (refused-p (lambda ()
                        (llm:with-ppq (:model "gpt-6.1-sol" :confidential nil :token-file *token*)
                          nil))))))
  (check "with-ppq: a private/ model under --confidential needs the Tinfoil trust"
    (and (let ((llm:*trust* nil))
           (refused-p (lambda ()
                        (llm:with-ppq (:model "private/gpt-oss-120b" :confidential t :token-file *token*)
                          nil))))
         (let ((llm:*trust* *tinfoil-trust*))
           (not (refused-p (lambda ()
                             (llm:with-ppq (:model "private/gpt-oss-120b" :confidential t :token-file *token*)
                               nil)))))))

  (check "claim-check!: a switch to a sealed, trusted path passes; to an unsealed one signals"
    (let ((llm:*confidential* t) (llm:*trust* *tinfoil-trust*))
      (and (llm:claim-check! :routstr "tinfoil-gemma4-31b")
           (refused-p (lambda () (llm:claim-check! :routstr "gemini-3-6-flash"))))))
  (check "claim-check!: with --confidential off, any switch passes"
    (let ((llm:*confidential* nil) (llm:*trust* nil))
      (llm:claim-check! :openrouter "deepseek/x")))
  (check "with-openrouter (a sub-agent's switch): refused under --confidential"
    (let ((llm:*confidential* t) (llm:*trust* *tinfoil-trust*))
      (refused-p (lambda ()
                   (llm:with-openrouter (:model "deepseek/x" :token-file *token*) nil)))))

  (check "sealer: enable-confidential! does not refuse a plain PPQ model; the claim decides"
    (let ((llm:*llm-backend* :ppq) (llm:*llm-model* "gpt-6.1-sol") (llm:*confidential* nil)
          (llm:*sealed-post* nil))
      (and (not (refused-p #'llm:enable-confidential!))
           (null llm:*sealed-post*))))

  (check "resume: confidential-flags restores the claim and its declarations"
    (let ((llm:*confidential* t) (llm:*trust* *tinfoil-trust*))
      (equal (llm:confidential-flags) "--confidential --trust=tinfoil-shared,tinfoil-inference")))
  (check "resume: no claim and no declarations means no flags"
    (let ((llm:*confidential* nil) (llm:*trust* nil))
      (null (llm:confidential-flags))))

  nil)

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&--confidential and --trust~%")
  (with-open-file (s *token* :direction :output :if-exists :supersede)
    (write-line "test-token" s))
  (unwind-protect (run-checks)
    (ignore-errors (delete-file *token*)))
  (format t "~&~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
