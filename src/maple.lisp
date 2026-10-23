;;;; maple.lisp — Maple's OpenSecret enclave, attested and encrypted from here.
;;;;
;;;; Maple's backend (OpenSecret, MaplePrivacyLabs/Maple services/opensecret) runs in an
;;;; AWS Nitro enclave at enclave.trymaple.ai.  We speak its transport directly, with no
;;;; maple-proxy in between:
;;;;
;;;;   GET  /attestation/<uuid nonce>   an AWS-signed Nitro document: PCRs, our nonce,
;;;;                                    and an X25519 key generated inside the enclave
;;;;   POST /key_exchange               {client_public_key, nonce} -> a session id and
;;;;                                    a session key sealed to our X25519 share
;;;;   POST /v1/chat/completions        {"encrypted": b64(nonce12 || ct || tag)} with
;;;;                                    ChaCha20-Poly1305 under the session key, both ways
;;;;
;;;; The document is verified locally (cl-sev-snp: signature, chain to the pinned AWS
;;;; root, our nonce), and its PCR0/1/2 must be one of the RELEASES below.  An enclave
;;;; we do not know is refused before anything is sent.
;;;;
;;;; WHY A TABLE.  The enclave decrypts our request and forwards it, in the clear inside
;;;; its own TLS, to a GPU provider.  Which provider is decided per MODEL by a route
;;;; table compiled into the image (provider_registry.rs: `const` routes, parsed "only
;;;; [from] compiled internal identities, never an arbitrary endpoint"); fallbacks stay
;;;; inside the model's own routes; sticky routes only repeat a route the table chose;
;;;; the remote feature-flag service only shows or hides models; the upstream URLs are
;;;; set by the image's own entrypoint (Continuum through the continuum-proxy binary in
;;;; the image, Tinfoil through the Tinfoil SDK in the image).  So a pinned image fixes,
;;;; per model, who can see the cleartext — and that is what --trust has to cover.
;;;;
;;;; Maple's own pages say only that "the model runs in its own attested enclave".  The
;;;; table below is what the source says instead.

(defpackage #:operandi.maple
  (:use #:cl)
  (:local-nicknames (#:json #:json-simple) (#:http #:operandi.http) (#:snp #:cl-sev-snp))
  (:export #:*enclave-base* #:*releases* #:release-pcr0 #:release-source
           #:model-providers #:model-trust #:known-model-p #:provider-trust
           #:enclave-post #:reset-session #:session-release))

(in-package #:operandi.maple)

(defvar *enclave-base* "https://enclave.trymaple.ai"
  "Maple's OpenSecret enclave.")

;;; ---- the pinned releases -------------------------------------------------------

(defparameter *releases*
  '((:pcr0 "832b7d6df249b80b3ae11c10972cf8e82dda2fed28873542f80ac431157749c758c0ab81e97736b9968086809819a570"
     :pcr1 "a91689e37a54303051f68263010f397ddfa92d66e17df7a03a6777c33ff1e4fc12b69bbbe786894d43dcba1cf6d60aaa"
     :pcr2 "9f7d3d49d4f05e6c808d0751e6b8ecd4e641e6dd71a6b84485ae7860fa4edf3faa746a2500f5d35cc6ee9af55b940e10"
     :source "4984a12f37c3ae93ec1b6ffbe0e6465114ac2e27"
     ;; "Approve prod OpenSecret EIF measurements from 4984a12f37c3", 2026-10-22.
     ;; Maple's own statement that this source builds to these PCRs; not yet rebuilt by us.
     :approved "87b7c49dc368f76d636417ee1e91c5c29019cd20"
     :reproduced nil
     ;; provider_registry.rs at SOURCE.  Weights are the code's comments, for the record;
     ;; only the SET of providers matters to the trust check, because fallback moves a
     ;; request to any healthy route of the same model.
     :routes (("gpt-oss-120b"                        :tinfoil)
              ("gemma4-31b"                          :tinfoil)
              ("kimi-k3"                             :tinfoil)
              ("llama3-3-70b"                        :tinfoil)
              ("gpt-oss-safeguard-120b"              :tinfoil)
              ("glm-5-3"                             :continuum :tinfoil)   ; 75% / 25%
              ("glm-5-3-flash"                       :continuum)
              ("deepseek-v4-1-flash"                 :maple :tinfoil)       ; i01 95% / 5%, and on i01 failure
              ("deepseek-v4-1-flash-1017-uncensored" :maple))))             ; i02 only, no fallback; per-account flag
  "Every OpenSecret image this client will talk to: its PCRs, the source Maple approved
   for them, and the providers each model can be sent to.  Add a release by reading
   provider_registry.rs at its source commit; an unknown PCR0 is refused.")

(defun release-pcr0 (release) (getf release :pcr0))
(defun release-source (release) (getf release :source))

(defun provider-trust (provider)
  "The --trust names that cover PROVIDER seeing the cleartext."
  (ecase provider
    (:tinfoil '("tinfoil-untrusted"))
    (:continuum '("continuum"))
    (:maple '("maple-inference"))))

(defun model-providers (model)
  "Every provider MODEL can reach under any pinned release, or NIL if no release
   routes it."
  (remove-duplicates
   (loop for r in *releases*
         append (rest (assoc model (getf r :routes) :test #'string=)))
   :from-end t))

(defun known-model-p (model)
  (some (lambda (r) (assoc model (getf r :routes) :test #'string=)) *releases*))

(defun model-trust (model)
  "The --trust names a confidential request for MODEL needs: Maple's own enclave plus
   whoever its route table can hand the cleartext to.  NIL for a model no pinned
   release routes."
  (and (known-model-p model)
       (remove-duplicates (cons "maple-routing"
                                (mapcan (lambda (p) (copy-list (provider-trust p)))
                                        (model-providers model)))
                          :test #'string= :from-end t)))

;;; ---- the session ----------------------------------------------------------------

(defstruct (session (:conc-name sess-)) id key release)

(defvar *session* nil "The current enclave session, or NIL.")
(defvar *session-lock* (bt:make-lock "maple-session"))

(defun reset-session () (bt:with-lock-held (*session-lock*) (setf *session* nil)))

(defun session-release ()
  "The release the current session was verified against, or NIL."
  (let ((s *session*)) (and s (sess-release s))))

(defun %u8 (x) (coerce x '(simple-array (unsigned-byte 8) (*))))
(defun %utf8 (s) (%u8 (babel:string-to-octets s :encoding :utf-8)))
(defun %str (o) (babel:octets-to-string (%u8 o) :encoding :utf-8))
(defun %hex (o) (string-downcase (snp:hex-string o)))
(defun %ht (&rest kv)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kv by #'cddr do (setf (gethash k h) v))
    h))

(defun %uuid4 ()
  (let ((b (%u8 (natrium:random-bytes 16))))
    (setf (aref b 6) (logior #x40 (logand (aref b 6) #x0f))
          (aref b 8) (logior #x80 (logand (aref b 8) #x3f)))
    (let ((h (%hex b)))
      (format nil "~a-~a-~a-~a-~a"
              (subseq h 0 8) (subseq h 8 12) (subseq h 12 16) (subseq h 16 20) (subseq h 20)))))

(defun %release-for (doc)
  "The pinned release whose PCR0, PCR1 and PCR2 all match DOC, or NIL."
  (let ((p0 (%hex (snp:nitro-doc-pcr doc 0)))
        (p1 (%hex (snp:nitro-doc-pcr doc 1)))
        (p2 (%hex (snp:nitro-doc-pcr doc 2))))
    (find-if (lambda (r) (and (string= p0 (getf r :pcr0))
                              (string= p1 (getf r :pcr1))
                              (string= p2 (getf r :pcr2))))
             *releases*)))

(defun %open (key b64)
  "Decrypt one {\"encrypted\": ...} payload, or signal."
  (let* ((e (%u8 (snp:base64-decode b64)))
         (n (length e)))
    (when (< n 28) (error "maple: encrypted payload too short (~d bytes)" n))
    (or (natrium:chacha20-poly1305-decrypt key (subseq e 0 12) (subseq e 12 (- n 16)) (subseq e (- n 16)))
        (error "maple: a response failed authentication under the session key"))))

(defun %seal (key plaintext)
  (let ((nonce (%u8 (natrium:random-bytes 12))))
    (multiple-value-bind (ct tag) (natrium:chacha20-poly1305-encrypt key nonce (%u8 plaintext))
      (snp:base64-encode (%u8 (concatenate '(vector (unsigned-byte 8)) nonce ct tag))))))

(defun %handshake (&key (base *enclave-base*))
  "Attest the enclave, check it is a pinned release, and agree a session key with the
   key the attestation vouches for.  Returns a SESSION; signals on any failure."
  (let* ((nonce (%uuid4))
         (body (http:get (format nil "~a/attestation/~a" base nonce) :read-timeout 30))
         (j (json:parse body))
         (doc-b64 (and (hash-table-p j) (gethash "attestation_document" j))))
    (unless (stringp doc-b64)
      (error "maple: ~a/attestation returned no attestation_document" base))
    (multiple-value-bind (doc ok checks)
        (snp:verify-nitro-doc (%u8 (snp:base64-decode doc-b64)) :nonce-hex (%hex (%utf8 nonce)))
      (unless (and doc ok)
        (error "maple: the enclave's Nitro attestation did not verify: ~{~a~^, ~}"
               (mapcar #'car (remove-if #'cdr checks))))
      (let ((release (%release-for doc)))
        (unless release
          (error "maple: the enclave runs an image this client does not know (PCR0 ~a). ~
                  Pinned: ~{~a~^, ~}.  Add it to OPERANDI.MAPLE:*RELEASES* only after reading ~
                  its route table."
                 (%hex (snp:nitro-doc-pcr doc 0)) (mapcar #'release-pcr0 *releases*)))
        (let ((server-pk (%u8 (snp:nitro-doc-public-key doc))))
          (unless (= (length server-pk) 32)
            (error "maple: the attested public key is ~d bytes, not an X25519 key" (length server-pk)))
          (multiple-value-bind (sk pk) (natrium:x25519-keypair)
            (unwind-protect
                 (let* ((resp (json:parse
                               (http:post (format nil "~a/key_exchange" base)
                                          :content (json:stringify
                                                    (%ht "client_public_key" (snp:base64-encode (%u8 pk))
                                                         "nonce" nonce))
                                          :headers '(("Content-Type" . "application/json"))
                                          :read-timeout 30)))
                        (sealed (gethash "encrypted_session_key" resp))
                        (sid (gethash "session_id" resp)))
                   (unless (and (stringp sealed) (stringp sid))
                     (error "maple: key_exchange returned no session"))
                   (let ((key (%open (%u8 (natrium:x25519 sk server-pk)) sealed)))
                     (unless (= (length key) 32)
                       (error "maple: the session key is ~d bytes" (length key)))
                     (make-session :id sid :key (%u8 key) :release release)))
              (fill sk 0))))))))

(defun %session (&key (base *enclave-base*))
  (bt:with-lock-held (*session-lock*)
    (or *session* (setf *session* (%handshake :base base)))))

(defun %model-of (body)
  (let ((j (ignore-errors (json:parse body))))
    (and (hash-table-p j) (gethash "model" j))))

(defun enclave-post (url body headers &key (base *enclave-base*) (read-timeout 300))
  "POST BODY (a chat-completions JSON string) to URL through the enclave session and
   return the decrypted response body as a string.  HEADERS supply Authorization.

   The request is refused before it is sent if the live release does not route the
   body's model: the trust check at startup was made against the pinned table, and a
   release that disagrees with it is not the one that check was about.  A response
   that is not encrypted under the session key is an error, never parsed."
  (let ((attempt 0))
    (loop
      (let* ((s (%session :base base))
             (model (%model-of body)))
        (unless (and model (assoc model (getf (sess-release s) :routes) :test #'string=))
          (error "maple: the enclave's release (source ~a) does not route model ~s"
                 (subseq (release-source (sess-release s)) 0 12) model))
        (handler-case
            (let* ((auth (assoc "Authorization" headers :test #'string-equal))
                   (resp (http:post url
                                    :content (json:stringify (%ht "encrypted" (%seal (sess-key s) (%utf8 body))))
                                    :headers (append '(("Content-Type" . "application/json"))
                                                     (list (cons "x-session-id" (sess-id s)))
                                                     (and auth (list auth)))
                                    :read-timeout read-timeout))
                   (j (ignore-errors (json:parse resp)))
                   (enc (and (hash-table-p j) (gethash "encrypted" j))))
              (unless (stringp enc)
                (error "maple: the enclave answered without encryption; refusing to read it"))
              (return (%str (%open (sess-key s) enc))))
          (http:http-request-failed (e)
            ;; A session can expire.  Start one fresh handshake and retry once; any
            ;; other failure is reported, decrypted if the enclave sealed it.
            (let* ((status (http:response-status e))
                   (j (ignore-errors (json:parse (http:response-body e))))
                   (enc (and (hash-table-p j) (gethash "encrypted" j)))
                   (detail (or (and (stringp enc) (ignore-errors (%str (%open (sess-key s) enc))))
                               (http:response-body e))))
              (if (and (member status '(400 401 403)) (< attempt 1)
                       (search "session" (string-downcase (princ-to-string detail))))
                  (progn (incf attempt) (reset-session))
                  (error "maple: HTTP ~a from the enclave: ~a" status detail)))))))))
