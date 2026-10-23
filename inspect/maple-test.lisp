;;; inspect/maple-test.lisp
;;;
;;; Oracle for the :MAPLE backend's two modes, its refusals and request shape.  No
;;; network: the backend is a switch, a route table and a request body.
;;;
;;; What it pins down:
;;;   - proxy mode: a non-loopback plaintext URL is refused and the backend does not
;;;     change; loopback with no token switches cleanly, with no Authorization key;
;;;     --confidential refuses it whatever --trust says
;;;   - enclave mode: needs a key; installs a sealer; --confidential requires exactly
;;;     the trust names the pinned route table gives the model, and refuses a model
;;;     no pinned release routes
;;;   - reasoning_effort is sent for low/medium/high and omitted for off/default
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

(defparameter *key-file*
  (format nil "~A/operandi-maple-test.token" (uiop:temporary-directory)))

(defun enclave-refused-p (model trust)
  "True when --confidential with --trust TRUST refuses MODEL on Maple's enclave."
  (let ((llm:*llm-backend* :maple) (llm:*maple-mode* :enclave) (llm:*llm-model* model)
        (llm:*confidential* t) (llm:*trust* trust))
    (handler-case (progn (llm:require-confidential!) nil)
      (error () t))))

(defparameter *no-token* "/nonexistent/operandi-test/maple.token"
  "A token file that is not there, so the backend has no key to send.")

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&maple backend~%")
  (with-open-file (k *key-file* :direction :output :if-exists :supersede)
    (write-line "test-key" k))
  (unwind-protect (run-checks)
    (ignore-errors (delete-file *key-file*)))
  (format t "~&~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(defun run-checks ()

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

  (check "proxy: under --confidential it switches, and startup refuses it whatever --trust says"
    (let ((llm:*llm-backend* :llama) (llm:*maple-mode* :enclave) (llm:*confidential* t)
          (llm:*trust* '("maple-routing" "tinfoil-untrusted" "maple-inference" "continuum")))
      (and (eq t (llm:set-backend-maple! :url llm:*maple-url* :token-file *no-token*))
           (eq llm:*llm-backend* :maple)
           (eq llm:*maple-mode* :proxy)
           (handler-case (progn (llm:require-confidential!) nil)
             (error () t)))))

  (check "proxy: loopback with no token file switches, with no key"
    (let ((llm:*llm-backend* :llama) (llm:*llm-url* nil) (llm:*llm-auth-token* "stale")
          (llm:*maple-mode* :enclave)
          (llm:*llm-model* nil) (llm:*confidential* nil) (llm:*sealed-post* :unset))
      (and (eq t (llm:set-backend-maple! :model "gpt-oss-120b" :url llm:*maple-url* :token-file *no-token*))
           (eq llm:*llm-backend* :maple)
           (equal llm:*llm-url* llm:*maple-url*)
           (null llm:*llm-auth-token*)
           (equal llm:*llm-model* "gpt-oss-120b")
           (null llm:*sealed-post*))))

  (check "cloud-backend-p: :maple is hosted and metered"
    (llm:cloud-backend-p :maple))
  (check "sealable-p: :maple through the proxy is never sealed by operandi"
    (let ((llm:*maple-mode* :proxy))
      (and (not (llm:sealable-p :maple "gpt-oss-120b"))
           (null (llm:path-trust :maple "gpt-oss-120b")))))

  (check "enclave: no key file is refused, and the backend does not change"
    (let ((llm:*llm-backend* :llama) (llm:*maple-mode* :proxy))
      (and (null (llm:set-backend-maple! :token-file *no-token*))
           (eq llm:*llm-backend* :llama)
           (eq llm:*maple-mode* :proxy))))
  (check "enclave: with a key it switches, sealed, to the enclave's endpoint"
    (let ((llm:*llm-backend* :llama) (llm:*maple-mode* :proxy) (llm:*llm-url* nil)
          (llm:*llm-auth-token* nil) (llm:*llm-model* nil) (llm:*sealed-post* nil))
      (and (eq t (llm:set-backend-maple! :model "deepseek-v4-1-flash" :token-file *key-file*))
           (eq llm:*maple-mode* :enclave)
           (functionp llm:*sealed-post*)
           (equal llm:*llm-url* "https://enclave.trymaple.ai/v1/chat/completions")
           (equal llm:*llm-auth-token* "test-key")
           (llm:sealable-p :maple "deepseek-v4-1-flash"))))

  ;; The live image, as the signed fixture describes it.  A fake session stands in for
  ;; the attestation so nothing here touches the network.
  (let* ((fixture (merge-pathnames "../t/fixtures/maple-routed-4984a12f.json"
                                   (directory-namestring *load-truename*)))
         (event (cl-nostr.event:json->event (uiop:read-file-string fixture)))
         (reader (cl-nostr.event:event-pubkey event))
         (live '("832b7d6df249b80b3ae11c10972cf8e82dda2fed28873542f80ac431157749c758c0ab81e97736b9968086809819a570"
                 "a91689e37a54303051f68263010f397ddfa92d66e17df7a03a6777c33ff1e4fc12b69bbbe786894d43dcba1cf6d60aaa"
                 "9f7d3d49d4f05e6c808d0751e6b8ecd4e641e6dd71a6b84485ae7860fa4edf3faa746a2500f5d35cc6ee9af55b940e10"))
         (other (list (make-string 96 :initial-element #\e) (second live) (third live))))
    (flet ((need (model &key (roots (list reader)) (pcrs live) (files (list fixture)))
             (let ((operandi.maple:*wot-roots* roots) (operandi.maple:*statement-files* files))
               (operandi.maple:required-trust model pcrs)))
           (refused-p (model trust &key (roots (list reader)))
             (let ((operandi.maple::*session* (operandi.maple::make-session :id "test" :key nil :pcrs live))
                   (operandi.maple:*wot-roots* roots) (operandi.maple:*statement-files* (list fixture)))
               (enclave-refused-p model trust))))
      (check "fixture: the signed routed record verifies"
        (equal (witness:statement-method (witness:event->statement event)) "routed"))
      (check "no web of trust: every model needs everyone Maple can send to"
        (and (equal (need "deepseek-v4-1-flash-1017-uncensored" :roots nil) operandi.maple:+all-trust+)
             (equal (need "glm-5-3-flash" :roots nil) operandi.maple:+all-trust+)))
      (check "a trusted routed record narrows each model to its own routes"
        (and (equal (need "gpt-oss-120b") '("maple-routing" "tinfoil-untrusted"))
             (equal (need "glm-5-3-flash") '("maple-routing" "continuum"))
             (equal (need "glm-5-3") '("maple-routing" "continuum" "tinfoil-untrusted"))
             (equal (need "deepseek-v4-1-flash") '("maple-routing" "maple-inference" "tinfoil-untrusted"))
             (equal (need "deepseek-v4-1-flash-1017-uncensored") '("maple-routing" "maple-inference"))))
      (check "a record about another image does not count for the live one"
        (equal (need "glm-5-3-flash" :pcrs other) operandi.maple:+all-trust+))
      (check "a record from outside the web of trust does not count"
        (equal (need "glm-5-3-flash" :roots (list (make-string 64 :initial-element #\7)))
               operandi.maple:+all-trust+))
      (check "a model the record does not list needs everyone"
        (equal (need "gpt-9") operandi.maple:+all-trust+))
      (check "a tampered record does not verify and does not count"
        (let ((bad (merge-pathnames "operandi-maple-tampered.json" (uiop:temporary-directory))))
          (unwind-protect
               (progn
                 (with-open-file (o bad :direction :output :if-exists :supersede)
                   (write-line (cl-ppcre:regex-replace "continuum" (uiop:read-file-string fixture) "tinfoil")
                               o))
                 (equal (need "glm-5-3-flash" :files (list bad)) operandi.maple:+all-trust+))
            (ignore-errors (delete-file bad)))))
      (check "--confidential: the uncensored DeepSeek needs only Maple's two names"
        (not (refused-p "deepseek-v4-1-flash-1017-uncensored" '("maple-routing" "maple-inference"))))
      (check "--confidential: stock DeepSeek also needs tinfoil-untrusted (5% and fallback)"
        (and (refused-p "deepseek-v4-1-flash" '("maple-routing" "maple-inference"))
             (not (refused-p "deepseek-v4-1-flash" '("maple-routing" "maple-inference" "tinfoil-untrusted")))))
      (check "--confidential: glm-5-3-flash needs continuum, not Tinfoil"
        (and (refused-p "glm-5-3-flash" '("maple-routing"))
             (not (refused-p "glm-5-3-flash" '("maple-routing" "continuum")))))
      (check "--confidential without a web of trust: only all four names will do"
        (and (refused-p "glm-5-3-flash" '("maple-routing" "continuum") :roots nil)
             (not (refused-p "glm-5-3-flash" operandi.maple:+all-trust+ :roots nil))))))

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
  nil)

(sb-ext:exit :code (if (run) 0 1))
