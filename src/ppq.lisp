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
           #:*tinfoil-base* #:*tinfoil-attestation-url* #:*tinfoil-token-file*
           #:ppq-error #:ppq-error-reason
           #:ppq-token #:ppq-model-p #:ppq-wire-model #:confidential-p
           #:ppq-price-usd #:*ppq-pricing*
           #:*routstr-tinfoil-prefix* #:routstr-confidential-p #:routstr-wire-model
           #:verify-attested-key
           #:sealed-post #:plain-post #:ppq-post #:ppq-chat #:tinfoil-chat
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

(defparameter *tinfoil-base* "https://inference.tinfoil.sh"
  "Base URL of Tinfoil's OWN inference endpoint.  Unlike PPQ's relay this is
   the enclave's front door: it serves /.well-known/hpke-keys and
   /v1/chat/completions, and its TLS certificate IS the attested enclave
   certificate (SPKI sha256 == report_data[0:32]).")

(defparameter *tinfoil-attestation-url* "https://atc.tinfoil.sh/attestation"
  "Where Tinfoil's attestation bundle comes from.  inference.tinfoil.sh does
   NOT serve /attestation (404); the bundle is built on demand by the
   Attestation Transparency service and must be asked for with a POST naming
   the enclave.  The GET ?domain= form is a trap: it silently ignores an
   unknown domain and hands back the router-0 bundle instead.")

(defparameter *tinfoil-token-file* (merge-pathnames ".operandi/tinfoil.token"
                                                    (user-homedir-pathname))
  "Where the Tinfoil API key lives (mode 600).")

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

;;; ---- pricing fallback ------------------------------------------------------
;;; PPQ reports `usage.cost` when the request asks with {"usage":{"include":true}}
;;; — but ONLY on the plaintext path.  The sealed Path A enclave (a vLLM behind
;;; Tinfoil) returns tokens and no cost at all, so a confidential run would show
;;; 0.000¢ forever.  For that case we price the turn ourselves from PPQ's public
;;; catalogue (GET /v1/models, no auth), which carries per-1M-token rates for
;;; every model including the private/* ones.  The table is fetched once and
;;; cached; a lookup miss returns NIL and the caller keeps the provider's 0.

(defvar *ppq-pricing* nil
  "Alist of (MODEL-ID . PLIST) from PPQ's catalogue, or NIL if never fetched.
   PLIST keys: :in :out :cache-read :cache-write (USD per 1M tokens).")

(defvar *ppq-pricing-fetched-p* nil
  "True once a fetch has been attempted, so a failure is not retried per turn.")

(defun %ppq-pricing-fetch ()
  "Fetch and cache PPQ's model catalogue pricing.  Best-effort: any failure
   leaves *PPQ-PRICING* NIL and the caller falls back to the provider's cost."
  (unless *ppq-pricing-fetched-p*
    (setf *ppq-pricing-fetched-p* t)
    (handler-case
        (let* ((parsed (%get-json (format nil "~A/v1/models" *ppq-base*)))
               (data (and (hash-table-p parsed) (%get parsed "data")))
               (acc '()))
          (when (vectorp data)
            (loop for m across data
                  when (hash-table-p m)
                    do (let ((id (%get m "id")) (p (%get m "pricing")))
                         (when (and (stringp id) (hash-table-p p))
                           (flet ((rate (k) (let ((v (%get p k)))
                                              (and (realp v) (coerce v 'double-float)))))
                             (push (cons id (list :in (rate "input_per_1M_tokens")
                                                  :out (rate "output_per_1M_tokens")
                                                  :cache-read (rate "cache_read_per_1M_tokens")
                                                  :cache-write (rate "cache_write_per_1M_tokens")))
                                   acc))))))
          (setf *ppq-pricing* acc))
      (error () nil)))
  *ppq-pricing*)

(defun ppq-price-usd (model prompt-tokens completion-tokens &key cached-tokens)
  "USD for a turn on MODEL, priced from PPQ's catalogue, or NIL when MODEL is
   unknown or the catalogue could not be fetched.  CACHED-TOKENS are billed at
   the cache-read rate and the rest of the prompt at the input rate."
  (let* ((entry (and (stringp model) (cdr (assoc model (%ppq-pricing-fetch) :test #'string=))))
         (in (getf entry :in))
         (out (getf entry :out)))
    (when (and in out)
      (let* ((cached (or cached-tokens 0))
             (fresh (max 0 (- (or prompt-tokens 0) cached)))
             (cr (or (getf entry :cache-read) in)))
        (/ (+ (* fresh in) (* cached cr) (* (or completion-tokens 0) out))
           1000000.0d0)))))

;;; ---- Routstr's Tinfoil models ----------------------------------------------
;;;
;;; Routstr is a network of independent Lightning-paid nodes.  A node that
;;; carries Tinfoil models exposes them as `tinfoil-<name>` and forwards EHBP
;;; bodies to the enclave as a BLIND RELAY: it never holds a key, it never
;;; decrypts, and it bills from the X-Tinfoil-Usage-Metrics header the enclave
;;; returns outside the sealed body (routstr/upstream/tinfoil.py: "Routstr acts
;;; as a blind relay: it forwards the opaque encrypted body, never sees
;;; plaintext").
;;;
;;; That makes the node itself a blind relay — but it does NOT make the path
;;; end-to-end attested, and the difference is the whole reason --confidential
;;; refuses :routstr.  The key a client can seal to is the ROUTER's
;;; (inference.tinfoil.sh), not the model enclave's: the router terminates our
;;; sealed body, then re-seals to the per-model container it picked.  So the
;;; router is inside the trust set, and the container it picked is exactly the
;;; thing in question.  The router does publish its view of that choice at
;;; /.well-known/tinfoil-proxy (repo, release tag, expected measurement, and per
;;; enclave the HPKE key + TLS fingerprint), and we have checked that the
;;; container's own TDX quote agrees with it — but the quote is TDX, which we
;;; cannot verify against a pinned root the way we verify the SEV-SNP one, and
;;; the manifest is served by the very enclave whose honesty is at issue.  So a
;;; Routstr request is ENCRYPTED, not ATTESTED.  Use --tinfoil for the attested
;;; path; use --routstr without --confidential for the encrypted one.
;;;
;;; The one thing the node DOES control is where the sealed body is forwarded
;;; (X-Tinfoil-Enclave-Url, restricted by the node to *.tinfoil.sh).  That is
;;; not a downgrade: a body sealed to the router's key is unreadable anywhere
;;; else, so a misrouted request fails rather than leaks.  We do not send the
;;; header at all, so the node uses its own configured enclave.

(defparameter *routstr-tinfoil-prefix* "tinfoil-"
  "The namespace a Routstr node puts on its Tinfoil-backed models.  It is the
   node's own convention (routstr/upstream/ehbp.py: TINFOIL_MODEL_PREFIX), and
   it is what tells us a model is served by the attested enclave rather than by
   some ordinary upstream the node terminates in the clear.")

(defun routstr-confidential-p (model)
  "True when MODEL, as named on a Routstr node, is served by the attested
   Tinfoil enclave — i.e. it carries the `tinfoil-` prefix.  Everything else a
   node offers (e2ee-*, plain vendor ids) is terminated by the node itself and
   is therefore NOT confidential, however the node markets it."
  (and (stringp model)
       (> (length model) (length *routstr-tinfoil-prefix*))
       (string-equal *routstr-tinfoil-prefix* model
                     :end2 (length *routstr-tinfoil-prefix*))))

(defun routstr-wire-model (model)
  "The model name the ENCLAVE expects inside the sealed body: MODEL with the
   `tinfoil-` prefix removed.  The full node-facing id travels in the
   X-Routstr-Model header, which the node strips before forwarding, so the
   enclave sees only the bare id it knows."
  (if (routstr-confidential-p model)
      (subseq model (length *routstr-tinfoil-prefix*))
      model))

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

(defun %post-json (url body)
  "POST BODY (a JSON string) to URL and parse the JSON answer.  Used for the
   one attestation endpoint that is a POST (Tinfoil's ATC)."
  (json:parse (http:post url :content body
                             :headers '(("Content-Type" . "application/json"))
                             :read-timeout 30)))

(defun %attestation-bundle (base &key attestation-url)
  "The attestation bundle for BASE.

   Two shapes, because the two providers publish it differently:
     * PPQ's relay serves the bundle itself at <base>/attestation (GET).
     * Tinfoil's enclave does not serve it at all; the bundle is built on
       demand by ATC, which must be POSTed the enclave URL.  ATTESTATION-URL
       names that endpoint, and the enclave URL is BASE."
  (if attestation-url
      (%post-json attestation-url
                  (with-output-to-string (s)
                    (json:stringify (%ht "enclaveUrl" base) :stream s)))
      (%get-json (format nil "~A/attestation" base))))

(defun verify-attested-key (&key (base *ppq-private-base*) attestation-url force)
  "Fetch BASE's attestation bundle and HPKE key, verify the bundle against the
   pinned AMD ARK, and check that the HPKE key is the one bound into the
   report.  Returns (values HPKE-KEY CHECKS BUNDLE).

   ATTESTATION-URL, when given, is where the bundle comes from instead of
   <BASE>/attestation (Tinfoil's enclave serves no /attestation; ATC builds
   one on demand from a POST).

   Every failure signals PPQ-ERROR.  There is no unverified path: a
   caller that cannot get a verified key does not get to send the request."
  (let ((key (list base attestation-url)))
    (when (and (not force) *attestation-cache*
               (equal key (getf *attestation-cache* :key))
               (< (- (get-universal-time) (getf *attestation-cache* :at)) *attestation-ttl*))
      (return-from verify-attested-key
        (values (getf *attestation-cache* :hpke)
                (getf *attestation-cache* :checks)
                (getf *attestation-cache* :bundle))))
    (let* ((bundle (handler-case (%attestation-bundle base :attestation-url attestation-url)
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

(defun attestation-status (&key (base *ppq-private-base*) attestation-url force)
  "Human-readable one-liner about BASE's attestation, for a banner or a check.
   Returns (values OK-P TEXT)."
  (handler-case
      (multiple-value-bind (key checks bundle)
          (verify-attested-key :base base :attestation-url attestation-url :force force)
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
                                            (model-header "X-Private-Model")
                                            attestation-url
                                            (read-timeout 240) (retries 2))
  "POST BODY (a JSON string) to URL with the request EHBP-sealed to BASE's
   attested HPKE key, and return the DECRYPTED response body as a string.

   HEADERS are the caller's headers; Content-Type and Authorization are kept,
   and Ehbp-Encapsulated-Key is added.  MODEL, when given, is the bare wire name
   and is written into the body and into MODEL-HEADER — X-Private-Model for
   PPQ's relay, X-Routstr-Model for a Routstr node.  Both are proxy control
   headers: the peer strips them before forwarding, so the enclave sees only
   the bare id inside the sealed body.

   The response must be EHBP-framed and carry Ehbp-Response-Nonce — anything
   else is a refusal, because a plaintext answer means the peer did not decrypt
   our sealed body.  A non-2xx status is likewise decrypted before it is
   reported: the enclave seals its errors too, and an undecrypted error body is
   unreadable noise."
  (let ((attempt 0))
    (loop
      (multiple-value-bind (hpke checks) (verify-attested-key :base base
                                                              :attestation-url attestation-url)
        (declare (ignore checks))
        (let* ((payload (if model (%rewrite-model body model) body))
               (req (seal:ehbp-seal-request hpke (%octets payload)))
               (enc (%hex (seal:ehbp-request-enc req)))
               (hdrs (append (remove-if (lambda (h) (string-equal (car h) "Ehbp-Encapsulated-Key"))
                                        headers)
                             `(("Ehbp-Encapsulated-Key" . ,enc))
                             (when model `((,model-header . ,model))))))
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
                                          attestation-url
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
                   :base private-base :attestation-url attestation-url
                   :model model :read-timeout read-timeout)
      (plain-post url body headers :read-timeout read-timeout)))

(defun ppq-chat (model prompt &key (system nil) (max-tokens 256) (temperature 0.0)
                                     confidential
                                     (base *ppq-base*) (private-base *ppq-private-base*)
                                     attestation-url
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
                           :attestation-url attestation-url
                           :model (and confidential wire)
                           :read-timeout read-timeout))
           (parsed (json:parse text))
           (choices (%get parsed "choices"))
           (msg (and (vectorp choices) (plusp (length choices))
                     (%get (aref choices 0) "message"))))
      (values (and msg (%get msg "content")) parsed))))

(defun tinfoil-chat (model prompt &key (system nil) (max-tokens 256) (temperature 0.0)
                                        (base *tinfoil-base*)
                                        (attestation-url *tinfoil-attestation-url*)
                                        (token-file *tinfoil-token-file*)
                                        (read-timeout 240))
  "One-shot chat against Tinfoil's OWN enclave (inference.tinfoil.sh), sealed
   end to end.  Returns (values TEXT PARSED).

   This is the same transport as confidential PPQ — EHBP to a locally verified
   AMD SEV-SNP enclave, no fallback — but pointed at the enclave instead of at
   PPQ's relay, so the only party in the path is the measured enclave.  MODEL
   is a bare Tinfoil id (gpt-oss-120b, glm-5-3, kimi-k3, …); there is no
   `private/` prefix here because there is no second path to distinguish it
   from.  The bundle comes from ATC (a POST), not from the enclave."
  (let* ((msgs (if system
                   (vector (%ht "role" "system" "content" system)
                           (%ht "role" "user" "content" prompt))
                   (vector (%ht "role" "user" "content" prompt))))
         (body (with-output-to-string (s)
                 (json:stringify (%ht
                                  "model" model "messages" msgs
                                  "max_tokens" max-tokens "temperature" temperature)
                                 :stream s)))
         (token (ppq-token token-file)))
    (unless token (refuse :no-token "no Tinfoil API key at ~a" token-file))
    (let* ((text (sealed-post (format nil "~A/v1/chat/completions" base) body
                              `(("Content-Type" . "application/json")
                                ("Authorization" . ,(concatenate 'string "Bearer " token)))
                              :base base :attestation-url attestation-url
                              :read-timeout read-timeout))
           (parsed (json:parse text))
           (choices (%get parsed "choices"))
           (msg (and (vectorp choices) (plusp (length choices))
                     (%get (aref choices 0) "message"))))
      (values (and msg (%get msg "content")) parsed))))
