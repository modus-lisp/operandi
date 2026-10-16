;;;; payments.lisp — ask for a Lightning payment and show it as a QR code.
;;;;
;;;; A Routstr node sells API balance for Lightning.  request-payment creates an
;;;; invoice (POST /v2/lightning/invoice), shows its bolt11 as a QR code, and polls
;;;; GET /v2/lightning/invoice/{id}/status until it is paid.  On payment the node
;;;; returns an api_key, which is written to the token file so :routstr can use it.
;;;;
;;;; Pure Common Lisp throughout: lispqr (QR matrix), png-pure (PNG bytes),
;;;; seal.http via operandi.http (the node).  The optional DESCRIPTION is local
;;;; text shown with the invoice; the node itself has no description field.

(defpackage #:operandi.payments
  (:use #:cl)
  (:local-nicknames (#:json #:json-simple) (#:http #:operandi.http))
  (:export #:create-invoice #:poll-invoice #:await-payment #:request-payment
           #:render-terminal #:open-image #:payment-error
           #:*routstr-node* #:*default-token-file*
           ;; PPQ (ppq.ai) Lightning top-up -- same shape, different endpoint.
           #:create-ppq-topup #:ppq-topup-status #:request-ppq-payment
           #:ppq-token #:*ppq-api* #:*ppq-token-file*
           #:ppq-create-account #:ppq-balance #:*ppq-credit-id-file* #:ppq-account-p))

(in-package #:operandi.payments)

(define-condition payment-error (error)
  ((message :initarg :message :reader payment-error-message))
  (:report (lambda (c s) (format s "payment: ~A" (payment-error-message c)))))

(defparameter *routstr-node* "https://routstr.cypherpunk.today"
  "Base URL of the Routstr provider node that issues the invoice.")

(defparameter *default-token-file*
  (merge-pathnames ".operandi/routstr.token" (user-homedir-pathname))
  "Where a paid api_key is stored for the :routstr backend.")

;;; ---------------------------------------------------------------
;;; PPQ (ppq.ai) -- Lightning top-up for an existing account
;;; ---------------------------------------------------------------
;;; PPQ sells API credit for Lightning: POST /topup/create/btc-lightning with
;;; {"amount":N,"currency":"SATS"} returns a bolt11 invoice (100..1,000,000 sats,
;;; 15-minute expiry); GET /topup/status/{invoice_id} reports payment.  Unlike
;;; Routstr the invoice does not mint a key -- the account already has one -- so
;;; there is nothing to store on payment.

(defparameter *ppq-api* "https://api.ppq.ai"
  "PPQ API base URL.")

(defparameter *ppq-token-file*
  (merge-pathnames ".operandi/ppq.token" (user-homedir-pathname))
  "PPQ API key (sk-...), read-only; authorizes the top-up.")

(defun ppq-token ()
  "The PPQ API key from *PPQ-TOKEN-FILE* (first line, trimmed)."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (with-open-file (s *ppq-token-file*)
                 (or (read-line s nil nil) ""))))

(defun %ppq-auth ()
  (list (cons "Authorization" (format nil "Bearer ~A" (ppq-token)))))

(defparameter *ppq-credit-id-file*
  (merge-pathnames ".operandi/ppq.credit_id" (user-homedir-pathname))
  "PPQ account credit id: the balance check and account-level calls name the
   account by it (the API key authorizes requests).")

(defun ppq-account-p ()
  "True when this machine has a PPQ API key."
  (and (probe-file *ppq-token-file*) (plusp (length (ppq-token)))))

(defun %write-secret (path text)
  (ensure-directories-exist path)
  (with-open-file (o path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line text o))
  #+sbcl (ignore-errors (funcall (find-symbol "CHMOD" "SB-POSIX") (namestring path) #o600)))

(defun ppq-create-account ()
  "Create a PPQ account (POST /accounts/create needs no credentials) and store
   its API key in *PPQ-TOKEN-FILE* and credit id in *PPQ-CREDIT-ID-FILE*.
   Refuses when a key is already there: a second account would split the
   balance, and the old key would be lost. Returns (values credit-id api-key)."
  (when (ppq-account-p)
    (error 'payment-error :message
           (format nil "a PPQ key already exists in ~A; not creating another account"
                   (namestring *ppq-token-file*))))
  #+sbcl (ignore-errors (require :sb-posix))
  (let* ((text (http:post (format nil "~A/accounts/create" *ppq-api*)
                          :content "{}" :headers '(("Content-Type" . "application/json"))
                          :read-timeout 25))
         (j (%parse text))
         (key (gethash "api_key" j))
         (cid (gethash "credit_id" j)))
    (unless (and (stringp key) (plusp (length key)))
      (error 'payment-error :message (format nil "PPQ account creation returned no api_key: ~A" text)))
    (%write-secret *ppq-token-file* key)
    (when (stringp cid) (%write-secret *ppq-credit-id-file* cid))
    (values cid key)))

(defun ppq-balance ()
  "The account balance as PPQ reports it (a hash table), or NIL without a
   credit id."
  (when (probe-file *ppq-credit-id-file*)
    (let* ((cid (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (with-open-file (s *ppq-credit-id-file*) (or (read-line s nil nil) ""))))
           (h (make-hash-table :test 'equal)))
      (setf (gethash "credit_id" h) cid)
      (%parse (http:post (format nil "~A/credits/balance" *ppq-api*)
                         :content (%json-body h) :headers '(("Content-Type" . "application/json"))
                         :read-timeout 20)))))

(defun create-ppq-topup (amount &key (unit :sats))
  "Create a PPQ Lightning top-up invoice for AMOUNT in base units: UNIT :sats
   (100..1,000,000) or :cents (USD cents). Integers only -- no decimal amounts
   anywhere; cents become PPQ's USD at the wire. Returns a plist: :invoice-id
   :bolt11 :amount :expires-at :checkout-url."
  (unless (and (integerp amount) (plusp amount))
    (error 'payment-error :message (format nil "PPQ amount must be a positive integer of ~(~A~), got ~A" unit amount)))
  (ecase unit
    (:sats (unless (<= 100 amount 1000000)
             (error 'payment-error :message (format nil "PPQ amount must be 100..1000000 sats, got ~A" amount))))
    (:cents nil))
  (let* ((currency (if (eq unit :sats) "SATS" "USD"))
         (wire (if (eq unit :sats)
                   amount
                   (multiple-value-bind (dollars rem) (floor amount 100)
                     (if (zerop rem) dollars (/ amount 100d0))))))
  (let* ((h (make-hash-table :test 'equal)))
    (setf (gethash "amount" h) wire
          (gethash "currency" h) currency)
    (let* ((text (http:post (format nil "~A/topup/create/btc-lightning" *ppq-api*)
                            :content (%json-body h)
                            :headers (cons (cons "Content-Type" "application/json")
                                           (%ppq-auth))
                            :read-timeout 25))
           (j (%parse text)))
      (unless (gethash "lightning_invoice" j)
        (error 'payment-error :message (format nil "PPQ top-up failed: ~A" text)))
      (list :invoice-id (gethash "invoice_id" j)
            :bolt11 (gethash "lightning_invoice" j)
            :amount (gethash "amount" j)
            :expires-at (gethash "expires_at" j)
            :checkout-url (gethash "checkout_url" j))))))

(defun ppq-topup-status (invoice-id)
  "Status plist for a PPQ top-up: :status (\"paid\"/...), :raw."
  (let* ((text (http:get (format nil "~A/topup/status/~A" *ppq-api* invoice-id)
                         :headers (%ppq-auth)
                         :read-timeout 20))
         (j (ignore-errors (%parse text))))
    (list :status (and (hash-table-p j)
                       (or (gethash "status" j) (gethash "payment_status" j)))
          :raw text)))

(defun %send-over-nostr (nip05 bolt11 &rest keys)
  "SEND-PAYMENT-REQUEST from the optional operandi/nostr system, loaded on
   first use so the core stays free of cl-nostr."
  (unless (find-package "OPERANDI.NOSTR")
    (let ((*standard-output* *error-output*))
      (funcall (or (and (find-package "QL") (find-symbol "QUICKLOAD" "QL"))
                   (lambda (s &rest r) (declare (ignore r)) (asdf:load-system s)))
               "operandi/nostr" :silent t)))
  (apply (find-symbol "SEND-PAYMENT-REQUEST" "OPERANDI.NOSTR") nip05 bolt11 keys))

(defun request-ppq-payment (amount &key (unit :sats) (provider :terminal) nip05 memo
                                        (wait t) (timeout 900) (interval 5)
                                        (out-path (merge-pathnames
                                                   "ppq-invoice.png"
                                                   (user-homedir-pathname))))
  "Create a PPQ Lightning top-up for AMOUNT base UNITs (:sats or :cents) and present it, then wait
   for payment. PROVIDER is :terminal (half-block QR), :open (PNG in the image
   viewer) or :nostr (a NIP-17 DM to NIP05 -- someone else pays, from their
   phone). Returns the paid status plist, or NIL on timeout/expiry."
  (let* ((inv (create-ppq-topup amount :unit unit))
         (bolt11 (getf inv :bolt11))
         (id (getf inv :invoice-id))
         (label (if (eq unit :cents)
                    (multiple-value-bind (d c) (floor amount 100) (format nil "$~D.~2,'0D" d c))
                    (format nil "~D sats" amount))))
    (format t "~&PPQ top-up: ~A~%invoice_id: ~A~%expires_at: ~A~%checkout:   ~A~%~%~A~%~%"
            label id (getf inv :expires-at) (getf inv :checkout-url) bolt11)
    (ecase provider
      (:terminal (render-terminal bolt11))
      (:open (open-image (%write-qr-png bolt11 out-path)))
      (:nostr
       (unless nip05 (error 'payment-error :message ":nostr needs a NIP-05 to send the invoice to"))
       (let ((relays (%send-over-nostr nip05 bolt11 :amount label
                                       :memo (or memo "PPQ API credit for operandi")
                                       :expires-at (getf inv :expires-at)
                                       :checkout-url (getf inv :checkout-url))))
         (format t "~&Sent to ~A over Nostr (accepted by ~{~A~^, ~}).~%" nip05 relays))))
    (unless wait (return-from request-ppq-payment inv))
    (format t "~&Waiting for payment (timeout ~Ds)...~%" timeout)
    (let ((deadline (+ (get-universal-time) timeout))
          (expires (let ((e (getf inv :expires-at))) (and (realp e) e))))
      (loop
        ;; PPQ invoices live ~15 minutes: past expiry nothing can pay this one,
        ;; so stop and let the caller issue another rather than wait out TIMEOUT
        (when (and expires (> (- (get-universal-time) 2208988800) expires))
          (format t "~&Invoice expired unpaid.~%")
          (return nil))
        (let ((st (ppq-topup-status id)))
          (when (and (stringp (getf st :status))
                     (member (string-downcase (getf st :status))
                             '("paid" "completed" "settled" "success")
                             :test #'equal))
            (format t "~&Paid.~%~A~%" (getf st :raw))
            (return st)))
        (when (> (get-universal-time) deadline)
          (format t "~&No payment received.~%")
          (return nil))
        (princ ".")
        (finish-output)
        (sleep interval)))))

(defun %json-body (hash)
  (json:stringify hash))

(defun %parse (text)
  (json:parse text))

(defun create-invoice (sats &key description)
  "Create a Lightning invoice for SATS on the node.  Returns a plist:
   :invoice-id :bolt11 :amount-sats :expires-at :description."
  (unless (and (integerp sats) (<= 1 sats 1000000))
    (error 'payment-error :message (format nil "amount_sats must be 1..1000000, got ~A" sats)))
  (let* ((body (let ((h (make-hash-table :test 'equal)))
                 (setf (gethash "amount_sats" h) sats
                       (gethash "purpose" h) "create")
                 (%json-body h)))
         (text (http:post (format nil "~A/v2/lightning/invoice" *routstr-node*)
                          :content body
                          :headers '(("Content-Type" . "application/json"))
                          :read-timeout 20))
         (j (%parse text)))
    (list :invoice-id (gethash "invoice_id" j)
          :bolt11 (gethash "bolt11" j)
          :amount-sats (gethash "amount_sats" j)
          :expires-at (gethash "expires_at" j)
          :description description)))

(defun poll-invoice (invoice-id)
  "Status of INVOICE-ID.  Returns a plist: :status (\"pending\"/\"paid\"/...),
   :api-key (NIL until paid), :paid-at."
  (let* ((text (http:get (format nil "~A/v2/lightning/invoice/~A/status"
                                 *routstr-node* invoice-id)
                         :read-timeout 20))
         (j (%parse text)))
    (list :status (gethash "status" j)
          :api-key (gethash "api_key" j)
          :paid-at (gethash "paid_at" j))))

(defun await-payment (invoice-id &key (timeout 900) (interval 3) (on-tick nil))
  "Poll INVOICE-ID until it is paid, expires, or TIMEOUT seconds pass.
   Returns the paid status plist, or NIL on timeout/expiry."
  (let ((deadline (+ (get-universal-time) timeout)))
    (loop
      (let ((st (poll-invoice invoice-id)))
        (when (equal (getf st :status) "paid")
          (return st))
        (when (member (getf st :status) '("expired" "cancelled") :test #'equal)
          (return nil))
        (when (> (get-universal-time) deadline)
          (return nil))
        (when on-tick (funcall on-tick st))
        (sleep interval)))))

;;; ---------------------------------------------------------------
;;; Rendering
;;; ---------------------------------------------------------------

(defun %qr-matrix (text)
  "2-D 1/0 array of the QR symbol for TEXT (no quiet zone), ECC level M."
  (funcall (symbol-function (find-symbol "ENCODE->MATRIX"
                                         (find-package "MARE5X.LISPQR.ENCODE")))
           text :ec-level :m))

(defun render-terminal (text &key (quiet 2) (stream *standard-output*))
  "Print TEXT as a QR code using half-block characters (two module rows per
   text line), so it fits a normal terminal without sixel.  Dark on light."
  (let* ((m (%qr-matrix text))
         (n (array-dimension m 0))
         (size (+ n (* 2 quiet))))
    (flet ((dark (r c)
             (let ((rr (- r quiet)) (cc (- c quiet)))
               (and (< -1 rr n) (< -1 cc n) (= 1 (aref m rr cc))))))
      ;; Each output line covers module rows 2y and 2y+1.
      (loop for y from 0 below (ceiling size 2)
            do (loop for x from 0 below size
                     do (let ((top (dark (* 2 y) x))
                              (bot (dark (1+ (* 2 y)) x)))
                          ;; Dark modules are drawn as blank and light ones as blocks,
                          ;; so the code reads correctly on a dark terminal background
                          ;; (where light modules glow).  Scanners accept either polarity.
                          (write-char (cond ((and (not top) (not bot)) (code-char #x2588))
                                            ((and (not top) bot) (code-char #x2580))
                                            ((and top (not bot)) (code-char #x2584))
                                            (t #\Space))
                                      stream)))
               (terpri stream)))
    (values)))

(defun %write-qr-png (text path &key (scale 8) (quiet 4))
  "Write TEXT's QR code as an 8-bit greyscale PNG at PATH."
  (let* ((m (%qr-matrix text))
         (n (array-dimension m 0))
         (side (* scale (+ n (* 2 quiet))))
         (px (make-array (* side side) :element-type '(unsigned-byte 8) :initial-element 255)))
    (dotimes (y side)
      (dotimes (x side)
        (let ((r (- (floor y scale) quiet))
              (c (- (floor x scale) quiet)))
          (when (and (< -1 r n) (< -1 c n) (= 1 (aref m r c)))
            (setf (aref px (+ (* y side) x)) 0)))))
    (funcall (symbol-function (find-symbol "WRITE-PNG-FILE"
                                           (find-package "PNG-PURE")))
             px side side path :channels 1)
    path))

(defun open-image (path)
  "Open PATH with the system image viewer: `open` on macOS, `xdg-open` elsewhere."
  (let ((cmd (if (uiop:os-macosx-p) "open" "xdg-open")))
    (uiop:run-program (list cmd (namestring path))
                      :ignore-error-status t)
    path))

;;; ---------------------------------------------------------------
;;; The hook
;;; ---------------------------------------------------------------

(defun %store-api-key (api-key token-file)
  (with-open-file (s token-file :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (write-string api-key s)
    (terpri s))
  ;; Keep the token readable only by its owner (it is a spending credential).
  (ignore-errors
   (uiop:run-program (list "chmod" "600" (namestring token-file))))
  token-file)

(defun request-payment (sats &key description (provider :terminal)
                                  (token-file *default-token-file*)
                                  (timeout 900)
                                  (out-path (merge-pathnames
                                             "operandi-invoice.png"
                                             (user-homedir-pathname))))
  "Request a Lightning payment of SATS, optionally with DESCRIPTION shown above the
   invoice.  PROVIDER is :terminal (half-block QR in the terminal) or :open (PNG
   written to OUT-PATH and opened in the system image viewer).  Waits for payment;
   on success writes the api_key to TOKEN-FILE and returns it.  Returns NIL if the
   invoice expires or TIMEOUT passes unpaid."
  (let* ((inv (create-invoice sats :description description))
         (bolt11 (getf inv :bolt11))
         (id (getf inv :invoice-id)))
    (format t "~&~@[~A~%~]Pay ~D sats (invoice ~A)~%~A~%~%"
            description sats id bolt11)
    (ecase provider
      (:terminal (render-terminal bolt11))
      (:open (open-image (%write-qr-png bolt11 out-path))))
    (format t "~&Waiting for payment (timeout ~Ds)...~%" timeout)
    (let ((paid (await-payment id :timeout timeout)))
      (cond ((and paid (getf paid :api-key))
             (%store-api-key (getf paid :api-key) token-file)
             (format t "~&Paid. API key saved to ~A~%" token-file)
             (getf paid :api-key))
            (t
             (format t "~&No payment received.~%")
             nil)))))
