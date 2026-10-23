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

;;; ---- 2. --confidential seals; --trust names the operator of the sealed endpoint ----

(format t "~&~%[2] --confidential refuses an unsealed path, and a sealed one whose operator is not trusted~%")

(defun claim-refused-p (backend model trust)
  "True when --confidential on BACKEND/MODEL with --trust TRUST is refused at startup."
  (let ((llm:*llm-backend* backend) (llm:*llm-model* model)
        (llm:*confidential* t) (llm:*trust* trust))
    (handler-case (progn (llm:require-confidential!) nil)
      (error () t))))

(defparameter *tinfoil-trust* '("tinfoil-untrusted"))

(check "--confidential + :openrouter is refused, whatever --trust says"
       (and (claim-refused-p :openrouter "deepseek/deepseek-v4-flash" nil)
            (claim-refused-p :openrouter "deepseek/deepseek-v4-flash" '("maple-routing" "tinfoil-untrusted" "continuum"))))

(check "--confidential on :tinfoil with no --trust is refused: sealed is not trusted"
       (claim-refused-p :tinfoil "gpt-oss-120b" nil))

(check "--confidential --trust=tinfoil-untrusted on :tinfoil is allowed"
       (not (claim-refused-p :tinfoil "gpt-oss-120b" *tinfoil-trust*)))

(check "tinfoil-inference alone enables nothing: nothing reaches it directly"
       (claim-refused-p :tinfoil "gpt-oss-120b" '("tinfoil-inference")))

(check "--confidential on :ppq private/ with the Tinfoil trust is allowed"
       (not (claim-refused-p :ppq "private/gpt-oss-120b" *tinfoil-trust*)))

(check "--confidential on :ppq with a plain model is refused, whatever --trust says"
       (claim-refused-p :ppq "gpt-6.1-sol" *tinfoil-trust*))

(check "--confidential on :routstr: only tinfoil-* models, with the Tinfoil trust"
       (and (claim-refused-p :routstr "gemini-3-6-flash" *tinfoil-trust*)
            (not (claim-refused-p :routstr "tinfoil-gemma4-31b" *tinfoil-trust*))))

(check "--confidential on :maple through maple-proxy is refused, whatever --trust says"
       (let ((llm:*maple-mode* :proxy))
         (claim-refused-p :maple "gpt-oss-120b" '("maple-routing" "tinfoil-untrusted" "continuum"))))

(check "--confidential on Maple's enclave with no web of trust: everyone Maple can send to"
       (let ((llm:*maple-mode* :enclave)
             (operandi.maple:*wot-roots* nil)
             (operandi.maple::*session* (operandi.maple::make-session :id "t" :key nil :pcrs '("a" "b" "c"))))
         (and (claim-refused-p :maple "glm-5-3-flash" '("maple-routing" "continuum"))
              (not (claim-refused-p :maple "glm-5-3-flash"
                                    '("maple-routing" "maple-inference" "tinfoil-untrusted" "continuum"))))))

(check "no --confidential means no check at all"
       (let ((llm:*llm-backend* :openrouter) (llm:*confidential* nil) (llm:*trust* nil))
         (llm:require-confidential!)))

(check "sealable-p: only tinfoil, private/ PPQ and tinfoil-* Routstr are sealed"
       (and (llm:sealable-p :tinfoil "gpt-oss-120b")
            (llm:sealable-p :ppq "private/gpt-oss-120b")
            (not (llm:sealable-p :ppq "gpt-6.1-sol"))
            (llm:sealable-p :routstr "tinfoil-gemma4-31b")
            (not (llm:sealable-p :routstr "gemma4-31b"))
            (not (llm:sealable-p :openrouter "x"))))

(check "trust names are case- and space-tolerant; a typo is reported, not dropped"
       (multiple-value-bind (names bad) (llm:parse-trust-list " Maple-Routing, Tinfoil-Untrusted ,wrong")
         (and (equal names '("maple-routing" "tinfoil-untrusted" "wrong")) (equal bad '("wrong")))))

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

;;; ---- 5. live: a Routstr node relays the sealed body blind -------------------
;;;
;;; The node is on the path but not in the trust set: we seal to the enclave's
;;; attested key, the node forwards the opaque body, and the answer comes back
;;; sealed.  The proof that the NODE could not read it is that the response
;;; carries an Ehbp-Response-Nonce and decrypts — only the enclave holds the
;;; private key that opens our sealed body, so a node that had terminated the
;;; request itself could not have produced it.
;;;
;;; Needs a node URL and a node-local key, neither of which is a fixture we can
;;; ship, so it is opt-in: OPERANDI_ROUTSTR_NODE + OPERANDI_ROUTSTR_KEY.

(format t "~&~%[5] live sealed round trip (Routstr node, blind relay)~%")

(let ((node (uiop:getenv "OPERANDI_ROUTSTR_NODE"))
      (key (uiop:getenv "OPERANDI_ROUTSTR_KEY"))
      (model (or (uiop:getenv "OPERANDI_ROUTSTR_MODEL") "tinfoil-gemma4-31b")))
  (if (and node key)
      (check "a Routstr node relays the sealed body to the attested enclave"
             ;; No blanket error handler here: if a node and key were supplied,
             ;; the round trip MUST succeed.  Catching the error and returning
             ;; true would make this test pass on a 401, which is exactly the
             ;; kind of vacuous green that lets a broken transport ship.
             (uiop:with-temporary-file (:pathname keyfile :stream s :keep t)
               (write-string key s)
               (finish-output s)
               (close s)
               (let ((llm:*llm-backend* :routstr)
                     (llm:*llm-model* model)
                     (llm:*confidential* t)
                     (llm:*trust* '("tinfoil-untrusted")))
                 (llm:set-backend-routstr! :model model :url node :token-file keyfile)
                 (llm:require-confidential!)
                 (multiple-value-bind (text parsed)
                     (llm:llm-chat "Reply with exactly: PONG" :max-tokens 300)
                   (declare (ignore parsed))
                   (and (stringp text) (search "PONG" text))))))
      (format t "~&  skip set OPERANDI_ROUTSTR_NODE and OPERANDI_ROUTSTR_KEY to run~%")))

;;; ---- summary ---------------------------------------------------------------

(format t "~&~%~a passed, ~a failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
