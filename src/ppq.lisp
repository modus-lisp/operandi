;;;; ppq.lisp — the :PPQ backend: PPQ's own API, optionally CONFIDENTIAL.
;;;;
;;;; WHAT THIS IS.  PPQ (ppq.ai) sells inference for Bitcoin/Lightning, which is
;;;; the only payment rail an agent can use without a human and a card.  It
;;;; offers two ways to reach a model, and they are NOT equally trustworthy:
;;;;
;;;;   Path A — the `private/*` models.  api.ppq.ai/private/* is a transparent
;;;;     relay to Tinfoil's enclave (inference.tinfoil.sh).  The relay publishes
;;;;     the enclave's HPKE key and its AMD SEV-SNP attestation bundle; the
;;;;     bundle is byte-identical to the one atc.tinfoil.sh serves, and the HPKE
;;;;     key is the one bound into the report's report_data[32:64].  So a client
;;;;     that verifies the bundle and seals to that key is talking, end to end,
;;;;     to a measured enclave — PPQ sees ciphertext and nothing else.
;;;;
;;;;   Path B — every other model.  The request is decrypted inside PPQ's OWN
;;;;     Nitro enclave and forwarded in plaintext to the upstream provider.  The
;;;;     enclave is attested, but its PCR0 is not independently reproducible and
;;;;     the second hop is not verified by the client, so "private" there means
;;;;     "PPQ promises not to look".
;;;;
;;;; TWO MODES, ONE BACKEND.  PPQ is a normal OpenAI-compatible provider for
;;;; its whole 381-model catalogue, and most of the time that is all you want:
;;;; `--ppq gpt-6.1-sol` posts to api.ppq.ai/v1/chat/completions in the clear,
;;;; exactly like OpenRouter.  What you must not be able to do is BELIEVE you
;;;; are confidential when you are not.  So confidentiality is a separate,
;;;; explicit switch — `--confidential` — and when it is on this file refuses
;;;; anything that is not Path A:
;;;;
;;;;   1. The model must be named `private/<tinfoil-model>`.  Anything else is
;;;;      Path B and is rejected before a byte leaves the process.
;;;;   2. The attestation bundle must verify against the pinned AMD ARK, and the
;;;;      HPKE key we seal to must be the one bound into report_data.  A bundle
;;;;      that does not verify is an error, not a warning.
;;;;   3. The response must carry an Ehbp-Response-Nonce header.  That header is
;;;;      only produced by a peer that decrypted our sealed body; its absence
;;;;      means the request was answered in plaintext by something that is not
;;;;      the enclave, and we refuse the answer rather than parse it.
;;;;
;;;; Refusal is the point: a provider that silently downgrades to plaintext when
;;;; attestation fails is worse than no provider, because the caller cannot tell
;;;; the difference.  Every failure here is a hard error, never a retry against
;;;; a lesser path.
;;;;
;;;; The verification itself is cl-sev-snp's (AMD SEV-SNP report chain, VCEK,
;;;; report signature, report_data binding, Sigstore DSSE statement) — the same
;;;; code that verifies Tinfoil directly, run against the relayed copy.

(defpackage #:operandi.ppq
  (:use #:cl)
  (:local-nicknames (#:json #:json-simple) (#:http #:operandi.http))
  (:export #:*ppq-base* #:*ppq-private-base* #:*ppq-token-file* #:*attestation-ttl*
           #:ppq-error #:ppq-error-reason
           #:ppq-token #:ppq-model-p #:ppq-wire-model #:confidential-p
           #:verify-attested-key
           #:sealed-post #:plain-post #:ppq-post #:ppq-chat
           #:attestation-status))

(in-package #:operandi.ppq)

;;; ---- configuration ---------------------------------------------------------

(defparameter *ppq-base* "https://api.ppq.ai"
  "Base URL of PPQ's ordinary OpenAI-compatible API.  This is the whole
   catalogue, in the clear — the same trust model as OpenRouter.")

(defparameter *ppq-private-base* "https://api.ppq.ai/private"
  "Base URL of PPQ's Path A relay.  /attestation, /.well-known/hpke-keys and
   /v1/chat/completions hang off this.  Only meaningful under :CONFIDENTIAL.")

(defparameter *ppq-token-file* (merge-pathnames ".operandi/ppq.token"
                                                (user-homedir-pathname))
  "Where the PPQ API key lives (mode 600).  PPQ mints these for free at
   POST https://api.ppq.ai/accounts/create; top up with Lightning.")

(defparameter *attestation-ttl* 300
  "Seconds an attestation result is reused.  The enclave's key is stable across
   fetches, but a stale key is a stale trust decision, so this is short.")

;;; ---- the refusal condition -------------------------------------------------

(define-condition ppq-error (error)
  ((reason :initarg :reason :reader ppq-error-reason)
   (detail :initarg :detail :initform nil :reader ppq-error-detail))
  (:report (lambda (c s)
             (format s "ppq refused (~a)~@[: ~a~]"
                     (ppq-error-reason c)
                     (ppq-error-detail c)))))

(defun refuse (reason fmt &rest args)
  "Signal a hard refusal.  There is deliberately no fallback path: a caller that
   wanted plaintext should have asked for a different backend."
  (error 'ppq-error :reason reason
                     :detail (and fmt (apply #'format nil fmt args))))

;;; ---- small helpers ---------------------------------------------------------

(defun %octets (x)
  (etypecase x
    (string (babel:string-to-octets x :encoding :utf-8))
    ((vector (unsigned-byte 8)) x)))

(defun %hex (v)
  (format nil "~(~{~2,'0x~}~)" (coerce v 'list)))

(defun %get (table key)
  "GETHASH with a string key, tolerating a NIL table."
  (and (hash-table-p table) (gethash key table)))

(defun %ht (&rest pairs)
  "A string-keyed hash table for json-simple serialization."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr do (setf (gethash k h) v))
    h))

(defun %unhex (s)
  (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return) s))
         (n (floor (length s) 2))
         (v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v)
      (setf (aref v i) (parse-integer s :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun ppq-token (&optional (file *ppq-token-file*))
  "The PPQ API key, or NIL.  Read fresh each call so a top-up that rewrites the
   file is picked up without restarting."
  (handler-case
      (with-open-file (s file)
        (let ((line (string-trim '(#\Space #\Tab #\Newline #\Return) (read-line s nil ""))))
          (and (plusp (length line)) line)))
    (error () nil)))

;;; ---- Path A model naming ---------------------------------------------------

(defparameter *private-prefix* "private/")

(defun ppq-model-p (model)
  "True when MODEL names a Path A (Tinfoil-relayed) model.  The `private/`
   prefix is the whole gate: it is what makes the choice of path explicit at
   the call site instead of a property of a lookup table we would have to keep
   in sync with PPQ's catalogue."
  (and (stringp model)
       (> (length model) (length *private-prefix*))
       (string-equal *private-prefix* model :end2 (length *private-prefix*))))

(defun ppq-wire-model (model)
  "The model name to put on the wire: MODEL with a leading `private/` removed.
   A non-private model is returned unchanged — it is a perfectly good model,
   just not a confidential one.  Whether that is ALLOWED is the caller's
   decision (see CONFIDENTIAL-P), not this function's."
  (if (ppq-model-p model)
      (subseq model (length *private-prefix*))
      model))

(defun confidential-p (model)
  "True when MODEL can be served confidentially, i.e. it is a Path A model.
   This is the gate :CONFIDENTIAL enforces; it is deliberately a property of
   the model NAME, so the choice of path is explicit at the call site instead
   of a lookup table we would have to keep in sync with PPQ's catalogue."
  (ppq-model-p model))

;;; ---- attestation -----------------------------------------------------------

(defvar *attestation-cache* nil
  "(:KEY KEY :BUNDLE BUNDLE :CHECKS CHECKS :AT UNIVERSAL-TIME) for the last
   successful verification, or NIL.")

(defun %get-octets (url)
  (multiple-value-bind (body code) (http:get url :read-timeout 30 :binary t)
    (declare (ignore code))
    (coerce body '(vector (unsigned-byte 8)))))

(defun %get-json (url)
  (json:parse (babel:octets-to-string (%get-octets url) :encoding :utf-8)))

(defun verify-attested-key (&key (base *ppq-private-base*) force)
  "Fetch BASE's attestation bundle and HPKE key, verify the bundle against the
   pinned AMD ARK, and check that the HPKE key is the one bound into the
   report.  Returns (values HPKE-KEY CHECKS BUNDLE).

   Every failure signals PPQ-ERROR.  There is no unverified path: a
   caller that cannot get a verified key does not get to send the request."
  (let ((key (list base)))
    (when (and (not force) *attestation-cache*
               (equal key (getf *attestation-cache* :key))
               (< (- (get-universal-time) (getf *attestation-cache* :at)) *attestation-ttl*))
      (return-from verify-attested-key
        (values (getf *attestation-cache* :hpke)
                (getf *attestation-cache* :checks)
                (getf *attestation-cache* :bundle))))
    (let* ((bundle (handler-case (%get-json (format nil "~A/attestation" base))
                     (error (e) (refuse :attestation-fetch "~a" e))))
           (cfg-octets (handler-case (%get-octets (format nil "~A/.well-known/hpke-keys" base))
                         (error (e) (refuse :hpke-fetch "~a" e))))
           (cfg (handler-case (seal:ehbp-parse-key-config cfg-octets)
                  (error (e) (refuse :hpke-parse "~a" e))))
           (hpke (seal:ehbp-key-config-public-key cfg)))
      ;; 1. The AMD chain, with the ARK pinned (cl-sev-snp's default pin).
      (multiple-value-bind (ask ark)
          (handler-case (cl-sev-snp:fetch-amd-chain)
            (error (e) (refuse :amd-chain "~a" e)))
        (multiple-value-bind (ok checks)
            (handler-case (cl-sev-snp:verify-tinfoil-bundle bundle ark ask)
              (error (e) (refuse :bundle-malformed "~a" e)))
          (unless ok
            (refuse :bundle-unverified "~{~a~^; ~}"
                    (cdr (assoc :failures checks))))
          ;; 2. The key we are about to seal to must be the attested one.
          (let ((rd (handler-case (cl-sev-snp:tinfoil-bundle-report-data bundle)
                      (error (e) (refuse :report-data "~a" e)))))
            (unless (equalp hpke (subseq rd 32 64))
              (refuse :key-not-attested
                      "HPKE key ~a is not report_data[32:64] (~a) — the relay is serving a key the enclave did not attest"
                      (%hex hpke) (%hex (subseq rd 32 64)))))
          (setf *attestation-cache*
                (list :key key :hpke hpke :checks checks :bundle bundle
                      :at (get-universal-time)))
          (values hpke checks bundle))))))

(defun attestation-status (&key (base *ppq-private-base*) force)
  "Human-readable one-liner about BASE's attestation, for a banner or a check.
   Returns (values OK-P TEXT)."
  (handler-case
      (multiple-value-bind (key checks bundle) (verify-attested-key :base base :force force)
        (values t (format nil "ppq confidential: ~a verified, HPKE key ~a, measurement ~a"
                          (or (%get bundle "domain") base)
                          (subseq (%hex key) 0 16)
                          (subseq (%hex (cl-sev-snp:snp-report-measurement
                                         (cl-sev-snp:parse-snp-report
                                          (cram:gzip-decompress
                                           (cl-sev-snp:base64-decode
                                            (%get (%get bundle "enclaveAttestationReport") "body"))))))
                                  0 16))))
    (ppq-error (e) (values nil (format nil "~a" e)))))

;;; ---- the sealed request ----------------------------------------------------

(defun %header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defun %rewrite-model (body wire)
  "Replace the \"model\" field of the JSON BODY with WIRE, keeping every other
   field exactly as the caller built it (tools, tool_choice, stream, …).  The
   caller names the model `private/<name>` (that is the Path A gate); the
   enclave wants the bare name, and X-Private-Model carries it too.  Rewriting
   here rather than at the call site keeps the private/ prefix visible in
   operandi's config, where it is the thing that says which path this is."
  (let* ((parsed (json:parse body))
         (out (%ht)))
    (when (hash-table-p parsed)
      (maphash (lambda (k v) (setf (gethash k out) v)) parsed))
    (setf (gethash "model" out) wire)
    (with-output-to-string (s) (json:stringify out :stream s))))

(defun %decrypt-or-note (req raw nonce)
  "Best-effort decrypt of a response body.  Returns the text, or NIL when RAW is
   not an EHBP frame we can open (a plaintext error page, say)."
  (when (and nonce raw (plusp (length raw)))
    (handler-case
        (multiple-value-bind (key nonce-base)
            (seal:ehbp-response-keys (seal:ehbp-request-context req)
                                     (seal:ehbp-request-enc req)
                                     (%unhex nonce))
          (seal:utf8-decode (seal:ehbp-open-response key nonce-base
                                                     (coerce raw '(vector (unsigned-byte 8))))))
      (error () nil))))

(defun sealed-post (url body headers &key (base *ppq-private-base*) (model nil)
                                            (read-timeout 240) (retries 2))
  "POST BODY (a JSON string) to URL with the request EHBP-sealed to BASE's
   attested HPKE key, and return the DECRYPTED response body as a string.

   HEADERS are the caller's headers; Content-Type and Authorization are kept,
   and Ehbp-Encapsulated-Key is added.  MODEL, when given, is the bare wire name
   and is written into the body and into X-Private-Model.

   The response must be EHBP-framed and carry Ehbp-Response-Nonce — anything
   else is a refusal, because a plaintext answer means the peer did not decrypt
   our sealed body.  A non-2xx status is likewise decrypted before it is
   reported: the enclave seals its errors too, and an undecrypted error body is
   unreadable noise."
  (let ((attempt 0))
    (loop
      (multiple-value-bind (hpke checks) (verify-attested-key :base base)
        (declare (ignore checks))
        (let* ((payload (if model (%rewrite-model body model) body))
               (req (seal:ehbp-seal-request hpke (%octets payload)))
               (enc (%hex (seal:ehbp-request-enc req)))
               (hdrs (append (remove-if (lambda (h) (string-equal (car h) "Ehbp-Encapsulated-Key"))
                                        headers)
                             `(("Ehbp-Encapsulated-Key" . ,enc))
                             (when model `(("X-Private-Model" . ,model))))))
          (handler-case
              (multiple-value-bind (raw code resp-headers)
                  (http:post url :content (seal:ehbp-request-body req)
                                :headers hdrs :read-timeout read-timeout :binary t)
                (declare (ignore code))
                (let ((nonce (%header resp-headers "Ehbp-Response-Nonce")))
                  (unless nonce
                    (refuse :plaintext-response
                            "~a answered without Ehbp-Response-Nonce — the sealed body was not decrypted by the attested enclave"
                            url))
                  ;; RETURN, not a bare value: this is inside LOOP, and a
                  ;; non-returning success would re-seal and re-post forever.
                  (return (or (%decrypt-or-note req raw nonce)
                              (refuse :response-undecryptable
                                      "~a returned a body that is not an EHBP frame for our request"
                                      url)))))
            (http:http-request-failed (e)
              (let* ((code (http:response-status e))
                     (nonce (%header (http:response-headers e) "Ehbp-Response-Nonce"))
                     (plain (%decrypt-or-note req (http:response-body e) nonce)))
                (cond
                  ;; A sealed error is the enclave talking to us; report it as
                  ;; the provider's message, not as a transport failure.
                  (plain (refuse :provider-error "HTTP ~a: ~a" code plain))
                  ((and (or (eql code 429) (and (integerp code) (>= code 500)))
                        (< attempt retries))
                   (sleep (min 30 (* 3 (expt 2 attempt)))) (incf attempt))
                  (t (error e)))))
            (ppq-error (e) (error e))
            (error (e)
              (if (< attempt retries)
                  (progn (sleep (min 30 (* 3 (expt 2 attempt)))) (incf attempt))
                  (error e)))))))))

(defun plain-post (url body headers &key (read-timeout 240) (retries 2))
  "POST BODY to URL in the clear, with the same retry policy as SEALED-POST.
   This is the ordinary PPQ path: PPQ terminates TLS and forwards to the
   upstream provider, exactly as OpenRouter would.  Nothing here is attested,
   which is why :CONFIDENTIAL never reaches it."
  (let ((attempt 0))
    (loop
      (handler-case
          (return (http:post url :content body :headers headers
                                 :read-timeout read-timeout))
        (http:http-request-failed (e)
          (let ((code (http:response-status e)))
            (if (and (or (eql code 429) (and (integerp code) (>= code 500)))
                     (< attempt retries))
                (progn (sleep (min 30 (* 3 (expt 2 attempt)))) (incf attempt))
                (error e))))
        (error (e)
          (if (< attempt retries)
              (progn (sleep (min 30 (* 3 (expt 2 attempt)))) (incf attempt))
              (error e)))))))

(defun ppq-post (url body headers &key confidential (base *ppq-base*)
                                          (private-base *ppq-private-base*)
                                          (model nil) (read-timeout 240))
  "The one place the two PPQ modes diverge.  CONFIDENTIAL routes through
   SEALED-POST against PRIVATE-BASE (attested, Path A only); otherwise through
   PLAIN-POST against BASE.

   URL is the caller's URL and is used as given when not confidential; under
   :CONFIDENTIAL it is rewritten onto PRIVATE-BASE, so a caller cannot
   accidentally seal a body to the enclave and then post it to the plaintext
   endpoint."
  (if confidential
      (sealed-post (if (and private-base (search "/private" url))
                       url
                       (format nil "~A/v1/chat/completions" private-base))
                   body headers
                   :base private-base :model model :read-timeout read-timeout)
      (plain-post url body headers :read-timeout read-timeout)))

(defun ppq-chat (model prompt &key (system nil) (max-tokens 256) (temperature 0.0)
                                     confidential
                                     (base *ppq-base*) (private-base *ppq-private-base*)
                                     (read-timeout 240))
  "One-shot chat against MODEL.  Returns (values TEXT PARSED).

   With CONFIDENTIAL, MODEL must be a `private/<name>` id and the exchange is
   EHBP-sealed to the attested enclave; without it, MODEL is any PPQ model id
   and the exchange is an ordinary HTTPS call.  A convenience for scripts and
   for the preflight check; the agent loop goes through
   OPERANDI.LLM:LLM-CHAT with the :PPQ backend."
  (when (and confidential (not (confidential-p model)))
    (refuse :not-path-a
            "~s is not a private/* model; Path B (PPQ's own enclave, plaintext upstream) is not allowed under --confidential"
            model))
  (let* ((wire (ppq-wire-model model))
         (msgs (if system
                   (vector (%ht "role" "system" "content" system)
                           (%ht "role" "user" "content" prompt))
                   (vector (%ht "role" "user" "content" prompt))))
         (body (with-output-to-string (s)
                 (json:stringify (%ht
                                  "model" wire "messages" msgs
                                  "max_tokens" max-tokens "temperature" temperature)
                                 :stream s)))
         (token (ppq-token)))
    (unless token (refuse :no-token "no PPQ API key at ~a" *ppq-token-file*))
    (let* ((text (ppq-post (format nil "~A/v1/chat/completions" base) body
                           `(("Content-Type" . "application/json")
                             ("Authorization" . ,(concatenate 'string "Bearer " token)))
                           :confidential confidential
                           :base base :private-base private-base
                           :model (and confidential wire)
                           :read-timeout read-timeout))
           (parsed (json:parse text))
           (choices (%get parsed "choices"))
           (msg (and (vectorp choices) (plusp (length choices))
                     (%get (aref choices 0) "message"))))
      (values (and msg (%get msg "content")) parsed))))
