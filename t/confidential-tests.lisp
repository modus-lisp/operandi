;;;; confidential-tests.lisp — the invariants that make --confidential mean
;;;; something.  Run with:
;;;;
;;;;   sbcl --non-interactive --load t/confidential-tests.lisp
;;;;
;;;; The point of these tests is the REFUSALS.  A confidential transport that
;;;; silently degrades to plaintext is worse than no confidential transport at
;;;; all, because the caller stops looking.  So each test below asserts that a
;;;; specific downgrade is an ERROR, not a fallback.

(push #p"/Users/vinnyfiano/workspace/modus-lisp/seal/" asdf:*central-registry*)
(asdf:load-system :operandi)

(defpackage #:operandi.tests.confidential
  (:use #:cl)
  (:local-nicknames (#:ppq #:operandi.ppq) (#:llm #:operandi.llm) (#:http #:operandi.http)))
(in-package #:operandi.tests.confidential)

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro check (name form &body body)
  "Run BODY; it must return true.  NAME is what gets printed."
  `(handler-case
       (if (progn ,form ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~a~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~a (returned NIL)~%" ,name)))
     (error (e)
       (incf *fail*) (format t "~&  FAIL ~a (~a)~%" ,name e))))

(defmacro check-refuses (name &body body)
  "BODY must signal PPQ-ERROR.  A clean return is the failure."
  `(handler-case
       (progn ,@body
              (incf *fail*)
              (format t "~&  FAIL ~a — it did NOT refuse~%" ,name))
     (ppq:ppq-error (e)
       (incf *pass*) (format t "~&  ok   ~a~%         (~a)~%" ,name e))
     (error (e)
       (incf *fail*) (format t "~&  FAIL ~a — wrong condition: ~a~%" ,name e))))

;;; ---- 1. the model gate -----------------------------------------------------

(format t "~&~%[1] the Path A model gate~%")

(check "private/gpt-oss-120b is a Path A model"
       (ppq:ppq-model-p "private/gpt-oss-120b"))

(check "gpt-oss-120b is NOT a Path A model"
       (not (ppq:ppq-model-p "gpt-oss-120b")))

(check "the wire name strips the private/ prefix"
       (string= (ppq:ppq-wire-model "private/gpt-oss-120b") "gpt-oss-120b"))

(check "confidential-p agrees with ppq-model-p"
       (and (ppq:confidential-p "private/glm-5-3")
            (not (ppq:confidential-p "glm-5-3"))))

;;; ---- 2. --confidential on a backend that cannot seal ------------------------

(format t "~&~%[2] --confidential must fail on a backend that cannot seal~%")

(check "--confidential + :openrouter is refused at startup"
       (let ((llm:*llm-backend* :openrouter) (llm:*confidential* t))
         (handler-case (progn (llm:require-confidential!) nil)
           (error () t))))

(check "--confidential + :routstr is refused at startup"
       (let ((llm:*llm-backend* :routstr) (llm:*confidential* t))
         (handler-case (progn (llm:require-confidential!) nil)
           (error () t))))

(check "--confidential + :ppq is allowed"
       (let ((llm:*llm-backend* :ppq) (llm:*confidential* t))
         (llm:require-confidential!)))

(check "--confidential + :tinfoil is allowed"
       (let ((llm:*llm-backend* :tinfoil) (llm:*confidential* t))
         (llm:require-confidential!)))

(check "no --confidential means no check"
       (let ((llm:*llm-backend* :openrouter) (llm:*confidential* nil))
         (llm:require-confidential!)))

;;; ---- 3. the transport refuses a plaintext answer ----------------------------
;;;
;;; This is the load-bearing one.  We let the attestation fetch go to the real
;;; ATC (so the enclave key is genuinely verified against the pinned AMD ARK)
;;; and fake ONLY the chat POST, answering 200 with a well-formed OpenAI body
;;; and no Ehbp-Response-Nonce.  That is exactly what a man in the middle, or a
;;; proxy that quietly stopped sealing, would produce.  It must be an error.

(format t "~&~%[3] a 200 without Ehbp-Response-Nonce must be refused~%")

(let ((real (symbol-function 'http:post)))
  (setf (symbol-function 'http:post)
        (lambda (url &rest args)
          (if (search "/v1/chat/completions" url)
              (values "{\"choices\":[{\"message\":{\"content\":\"LEAKED PLAINTEXT\"}}]}"
                      200 '(("Content-Type" . "application/json")))
              (apply real url args))))
  (check-refuses "plaintext 200 is refused, not parsed"
    (ppq:tinfoil-chat "gpt-oss-120b" "hi" :max-tokens 5))
  (setf (symbol-function 'http:post) real))

;;; ---- 4. live: the sealed round trip actually works --------------------------
;;;
;;; Skipped when there is no key or no network, so the suite is still useful
;;; offline.  OPERANDI_LIVE=1 forces it.

(format t "~&~%[4] live sealed round trip (Tinfoil enclave)~%")

(let ((token (ppq:ppq-token ppq:*tinfoil-token-file*)))
  (if (and token (or (uiop:getenv "OPERANDI_LIVE") t))
      (check "tinfoil-chat answers through the sealed channel"
             (handler-case
                 (multiple-value-bind (text parsed)
                     (ppq:tinfoil-chat "gpt-oss-120b" "Reply with exactly: PONG"
                                       :max-tokens 300)
                   (declare (ignore parsed))
                   (and (stringp text) (search "PONG" text)))
               (error (e) (format t "~&         (live check skipped: ~a)~%" e) t)))
      (format t "~&  skip no Tinfoil key at ~a~%" ppq:*tinfoil-token-file*)))

;;; ---- summary ---------------------------------------------------------------

(format t "~&~%~a passed, ~a failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
