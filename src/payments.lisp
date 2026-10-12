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
           #:*routstr-node* #:*default-token-file*))

(in-package #:operandi.payments)

(define-condition payment-error (error)
  ((message :initarg :message :reader payment-error-message))
  (:report (lambda (c s) (format s "payment: ~A" (payment-error-message c)))))

(defparameter *routstr-node* "https://routstr.cypherpunk.today"
  "Base URL of the Routstr provider node that issues the invoice.")

(defparameter *default-token-file*
  (merge-pathnames ".operandi/routstr.token" (user-homedir-pathname))
  "Where a paid api_key is stored for the :routstr backend.")

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
