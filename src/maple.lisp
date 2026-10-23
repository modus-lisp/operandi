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
;;;; root, our nonce) before anything is sent.
;;;;
;;;; WHO SEES THE CLEARTEXT NEXT.  The enclave decrypts our request and forwards it to a
;;;; GPU provider, chosen per model by a route table compiled into its image
;;;; (provider_registry.rs).  The table changes with Maple's releases, every few days, so
;;;; it is not kept here: it is a `routed' statement (witness, DEP-14 kind 55502) about
;;;; the image's PCRs, signed by whoever read the source.  When enough readers in your web
;;;; of trust agree on where the live image sends your model, that is what --trust has to
;;;; cover.  Otherwise it is everyone Maple can send to (+ALL-PROVIDERS+).

(defpackage #:operandi.maple
  (:use #:cl)
  (:local-nicknames (#:json #:json-simple) (#:http #:operandi.http) (#:snp #:cl-sev-snp)
                    (#:w #:witness))
  (:export #:*enclave-base* #:*statement-files* #:*wot-roots* #:*threshold*
           #:+all-trust+ #:provider-trust #:live-routes #:required-trust #:routes-explanation
           #:live-pcrs #:enclave-post #:reset-session #:*on-new-enclave*))

(in-package #:operandi.maple)

(defvar *enclave-base* "https://enclave.trymaple.ai"
  "Maple's OpenSecret enclave.")

(defvar *service* "enclave.trymaple.ai"
  "The service name `routed' statements about this enclave carry.")

;;; ---- where the cleartext can go -------------------------------------------------

(defparameter +all-trust+ '("maple-routing" "maple-inference" "tinfoil-untrusted" "continuum")
  "Everyone Maple's enclave can hand a request to, as --trust names: required when no
   `routed' statement in the web of trust covers the model on the live image.")

(defun provider-trust (provider)
  "The --trust name covering PROVIDER (OpenSecret's own name for it), or NIL if there is
   none: a provider with no name can never be covered."
  (cond ((string= provider "tinfoil") "tinfoil-untrusted")
        ((string= provider "continuum") "continuum")
        ((uiop:string-prefix-p "maple-" provider) "maple-inference")
        (t nil)))

(defvar *statement-files*
  (append (list (merge-pathnames ".local/share/witness/statements.jsonl" (user-homedir-pathname)))
          (let ((env (uiop:getenv "OPERANDI_STATEMENTS")))
            (and env (remove "" (uiop:split-string env :separator ":") :test #'string=))))
  "Where `routed' (and `proclaim') statements are read from: the witness store, plus
   the files in OPERANDI_STATEMENTS (colon-separated).  Each holds one event per line.
   Nothing is fetched from relays yet.")

(defvar *wot-roots*
  (let ((env (uiop:getenv "OPERANDI_WOT_ROOTS")))
    (and env (remove "" (mapcar (lambda (s) (string-trim " " s))
                                (uiop:split-string env :separator ","))
                     :test #'string=)))
  "The pubkeys (hex or npub) whose web of trust decides which `routed' statements count.
   From OPERANDI_WOT_ROOTS.  NIL: none count, and every model needs +ALL-TRUST+.")

(defvar *threshold* 1 "Distinct trusted readers who must agree on a model's routes.")

(defun %load-statements ()
  (loop for f in *statement-files*
        when (probe-file f)
          append (with-open-file (in f)
                   (loop for line = (read-line in nil) while line
                         for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
                         when (plusp (length trimmed))
                           append (handler-case
                                      (list (w:event->statement (cl-nostr.event:json->event trimmed)))
                                    (error () nil))))))

(defun live-routes (pcrs model)
  "Where the image measured as PCRS (PCR0 PCR1 PCR2, hex) sends MODEL, according to the
   web of trust.  Returns (values PROVIDERS SIGNERS CONFLICT): PROVIDERS is NIL when no
   agreement covers it."
  (if (null *wot-roots*)
      (values nil nil nil)
      (let ((policy (w:make-policy (%load-statements) :roots *wot-roots*)))
        (w:routed-providers policy (format nil "~(~{~a~^|~}~)" pcrs) model
                            :service *service* :threshold *threshold*))))

(defun required-trust (model &optional (pcrs (live-pcrs)))
  "The --trust names a confidential request for MODEL to the image PCRS needs.  Returns
   (values NAMES HOW): HOW is :ROUTED when a web-of-trust agreement narrowed it, else
   :DEFAULT (everyone Maple can send to).  Signals when the agreed routes name a provider
   no --trust name covers."
  (multiple-value-bind (providers) (live-routes pcrs model)
    (if (null providers)
        (values +all-trust+ :default)
        (values (remove-duplicates
                 (cons "maple-routing"
                       (mapcar (lambda (p)
                                 (or (provider-trust p)
                                     (error "maple: the routes for ~a include ~s, which no --trust name covers"
                                            model p)))
                               providers))
                 :test #'string= :from-end t)
                :routed))))

(defun routes-explanation (model &optional (pcrs (live-pcrs)))
  "One line saying where MODEL's route requirement came from, for refusal messages."
  (multiple-value-bind (providers signers conflict) (live-routes pcrs model)
    (cond (providers
           (format nil "~a on the live image (PCR0 ~a…) goes to ~{~a~^, ~}, per ~{~a~^, ~}"
                   model (subseq (first pcrs) 0 12) providers
                   (mapcar (lambda (s) (subseq (w:key-npub s) 0 16)) signers)))
          (conflict
           (format nil "readers in your web of trust disagree about where ~a goes on the live image (PCR0 ~a…), so Maple may send it anywhere it routes"
                   model (subseq (first pcrs) 0 12)))
          ((null *wot-roots*)
           (format nil "no web of trust is configured (OPERANDI_WOT_ROOTS), so Maple may send ~a anywhere it routes"
                   model))
          (t
           (format nil "no `routed' statement in your web of trust covers ~a on the live image (PCR0 ~a…), so Maple may send it anywhere it routes"
                   model (subseq (first pcrs) 0 12))))))

;;; ---- the session ----------------------------------------------------------------

(defstruct (session (:conc-name sess-)) id key pcrs)

(defvar *session* nil "The current enclave session, or NIL.")
(defvar *session-lock* (bt:make-lock "maple-session"))
(defvar *checked-pcrs* nil "The image *ON-NEW-ENCLAVE* last accepted.")

(defvar *on-new-enclave* nil
  "Called with no arguments, outside the session lock, the first time a session is with
   an image other than the last one accepted.  It signals to refuse the image: the
   check made against the old image is not a check of this one.")

(defun reset-session ()
  (bt:with-lock-held (*session-lock*) (setf *session* nil)))

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
  "Attest the enclave and agree a session key with the key the attestation vouches for.
   Returns a SESSION; signals on any failure."
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
      (let ((server-pk (%u8 (snp:nitro-doc-public-key doc)))
            (pcrs (loop for i below 3 collect (%hex (snp:nitro-doc-pcr doc i)))))
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
                   (make-session :id sid :key (%u8 key) :pcrs pcrs)))
            (fill sk 0)))))))

(defun %session (&key (base *enclave-base*))
  (let ((s (bt:with-lock-held (*session-lock*)
             (or *session* (setf *session* (%handshake :base base))))))
    (unless (equal (sess-pcrs s) *checked-pcrs*)
      (when *on-new-enclave* (funcall *on-new-enclave*))
      (setf *checked-pcrs* (sess-pcrs s)))
    s))

(defun live-pcrs ()
  "PCR0, PCR1, PCR2 of the enclave, attesting it if there is no session yet."
  (sess-pcrs (bt:with-lock-held (*session-lock*)
               (or *session* (setf *session* (%handshake))))))

(defun enclave-post (url body headers &key (base *enclave-base*) (read-timeout 300))
  "POST BODY (a chat-completions JSON string) to URL through the enclave session and
   return the decrypted response body as a string.  HEADERS supply Authorization.  A
   response that is not encrypted under the session key is an error, never parsed."
  (let ((attempt 0))
    (loop
      (let ((s (%session :base base)))
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
