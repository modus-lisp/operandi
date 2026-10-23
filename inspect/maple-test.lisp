;;; inspect/maple-test.lisp
;;;
;;; Oracle for the :MAPLE backend's refusals and request shape.  No network: the
;;; backend is a switch plus a request body, so both are checked directly.
;;;
;;; What it pins down:
;;;   - a non-loopback plaintext URL is refused, and the backend does not change
;;;   - a loopback URL with no token file switches cleanly, with no Authorization key
;;;   - --confidential is a claim checked at startup, not a setter refusal
;;;   - reasoning_effort is sent for low/medium/high and omitted for off/default
;;;   - :MAPLE is a cloud backend and is never a confidential one
;;;
;;;   sbcl --non-interactive --load inspect/maple-test.lisp   (exits 0/1)

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.maple-test
  (:use #:cl)
  (:local-nicknames (#:llm #:operandi.llm))
  (:export #:run))
(in-package #:operandi.maple-test)

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro check (name &body body)
  `(handler-case
       (if (progn ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A~%" ,name)))
     (serious-condition (e)
       (incf *fail*) (format t "~&  FAIL ~A (~A)~%" ,name (type-of e)))))

(defparameter *no-token* "/nonexistent/operandi-test/maple.token"
  "A token file that is not there, so the backend has no key to send.")

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&maple backend~%")

  (check "loopback: 127.0.0.1 with a path"
    (llm:maple-loopback-url-p "http://127.0.0.1:8080/v1/chat/completions"))
  (check "loopback: localhost on another port"
    (llm:maple-loopback-url-p "http://localhost:9000/v1/chat/completions"))
  (check "loopback: IPv6 [::1] with a port"
    (llm:maple-loopback-url-p "http://[::1]:8080/v1/chat/completions"))
  (check "not loopback: a LAN host"
    (not (llm:maple-loopback-url-p "http://192.168.1.20:8080/v1/chat/completions")))
  (check "not loopback: a hostname that merely starts with 127"
    (not (llm:maple-loopback-url-p "http://127.0.0.1.example.com:8080/v1")))

  (check "refusal: plaintext http to a non-loopback host"
    (stringp (llm:maple-refusal "http://192.168.1.20:8080/v1/chat/completions")))
  (check "no refusal: https to a remote host (TLS in front of the proxy)"
    (null (llm:maple-refusal "https://maple.example.net/v1/chat/completions")))
  (check "no refusal: plaintext to loopback"
    (null (llm:maple-refusal "http://127.0.0.1:8080/v1/chat/completions")))

  (check "set-backend-maple!: refuses a plaintext remote URL and leaves the backend alone"
    (let ((llm:*llm-backend* :llama) (llm:*llm-url* "http://localhost:9931/v1/chat/completions")
          (llm:*confidential* nil))
      (and (null (llm:set-backend-maple! :url "http://192.168.1.20:8080/v1/chat/completions"
                                         :token-file *no-token*))
           (eq llm:*llm-backend* :llama)
           (equal llm:*llm-url* "http://localhost:9931/v1/chat/completions"))))

  (check "set-backend-maple!: under --confidential it switches, and startup refuses it"
    (let ((llm:*llm-backend* :llama) (llm:*confidential* t) (llm:*trust* nil))
      (and (eq t (llm:set-backend-maple! :token-file *no-token*))
           (eq llm:*llm-backend* :maple)
           (handler-case (progn (llm:require-confidential!) nil)
             (error () t)))))

  (check "--confidential on :maple is refused even with all three trusted: the proxy is unverified"
    (let ((llm:*llm-backend* :maple) (llm:*llm-model* "gpt-oss-120b") (llm:*confidential* t)
          (llm:*trust* '("maple" "tinfoil-shared" "tinfoil-inference")))
      (handler-case (progn (llm:require-confidential!) nil)
        (error () t))))

  (check "set-backend-maple!: loopback with no token file switches, with no key"
    (let ((llm:*llm-backend* :llama) (llm:*llm-url* nil) (llm:*llm-auth-token* "stale")
          (llm:*llm-model* nil) (llm:*confidential* nil) (llm:*sealed-post* :unset))
      (and (eq t (llm:set-backend-maple! :model "gpt-oss-120b" :token-file *no-token*))
           (eq llm:*llm-backend* :maple)
           (equal llm:*llm-url* llm:*maple-url*)
           (null llm:*llm-auth-token*)
           (equal llm:*llm-model* "gpt-oss-120b")
           (null llm:*sealed-post*))))

  (check "cloud-backend-p: :maple is hosted and metered"
    (llm:cloud-backend-p :maple))
  (check "sealable-p: :maple through the proxy is never sealed by operandi"
    (and (not (llm:sealable-p :maple "gpt-oss-120b"))
         (null (llm:path-trust :maple "gpt-oss-120b"))))

  (flet ((effort-body (effort)
           (let ((body (llm:ht "messages" #())))
             (llm:apply-reasoning body effort)
             body)))
    (let ((llm:*llm-backend* :maple))
      (check "reasoning: low/medium/high send reasoning_effort"
        (and (equal (gethash "reasoning_effort" (effort-body :low)) "low")
             (equal (gethash "reasoning_effort" (effort-body :medium)) "medium")
             (equal (gethash "reasoning_effort" (effort-body :high)) "high")))
      (check "reasoning: off and default send no reasoning field at all"
        (let ((off (effort-body :off)) (dflt (effort-body nil)))
          (and (null (gethash "reasoning_effort" off))
               (null (gethash "reasoning" off))
               (null (gethash "chat_template_kwargs" off))
               (null (gethash "reasoning_effort" dflt)))))))
  (format t "~&~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
