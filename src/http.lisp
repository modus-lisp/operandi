;;;; http.lisp — the ONE place operandi speaks HTTP.
;;;;
;;;; WHY THIS EXISTS.  operandi used dexador, and dexador was the only route by which foreign
;;;; code entered the stack: cl+ssl (OpenSSL through CFFI), iolib, and static-vectors — over thirty
;;;; of the fifty-odd systems operandi loaded, for a client that makes JSON POSTs and reads a
;;;; server-sent-event stream.  Everything here sits on seal.http instead: TLS on natrium, pure
;;;; Common Lisp end to end, which is what lets operandi move to an implementation that has no
;;;; FFI at all.
;;;;
;;;; THE CONTRACT the call sites depend on, and the reason for each part of it:
;;;;
;;;;   * A non-2xx status SIGNALS HTTP-REQUEST-FAILED, carrying the status and the body as text.
;;;;     The engine's retry loop classifies on the status (429 and 5xx retry, other 4xx stop), and
;;;;     provider errors — "no allowed providers", an exhausted grant — are only legible from the
;;;;     body.  A client that returned the error page as if it were the answer would turn every
;;;;     provider refusal into "[empty response from model]".
;;;;   * :WANT-STREAM returns a character stream over the body, for SSE; the caller CLOSEs it.  A
;;;;     streamed request that fails is still read far enough to put its body in the condition.
;;;;   * :MAX-BYTES stops reading a body after N octets, for a fetch that must not pull an
;;;;     endless page into memory.
;;;;   * :READ-TIMEOUT is per receive, not per request — a stream that keeps producing is never
;;;;     cut off, only one that goes silent.
;;;;
;;;; Text is decoded as UTF-8 leniently (a bad byte becomes a replacement character rather than an
;;;; error), by the UTF-8 code below -- portable, and not an implementation's own octet functions.

(defpackage #:operandi.http
  (:use #:cl)
  (:shadow #:get)
  (:export #:get #:post #:request
           #:http-request-failed #:response-status #:response-body #:response-url
           #:response-headers
           #:utf8-octets #:utf8-string #:base64-string))

(in-package #:operandi.http)

(define-condition http-request-failed (error)
  ((status :initarg :status :reader response-status)
   (body   :initarg :body   :reader response-body)
   (url    :initarg :url    :reader response-url)
   (headers :initarg :headers :initform nil :reader response-headers))
  (:report (lambda (c s)
             (let ((b (response-body c)))
               (format s "HTTP ~a from ~a~@[: ~a~]" (response-status c) (response-url c)
                       (cond ((not (stringp b)) nil)
                             ((plusp (length b)) (subseq b 0 (min 200 (length b))))))))))

(defparameter *error-body-cap* 65536
  "How much of a FAILED streamed response is read into the condition.  Error bodies are small;
   the cap only matters if a server answers a stream with a failure and then keeps talking.")

;;; UTF-8, both ways, in plain Lisp.  All operandi ever asked of babel: its dozens of other encodings were
;;; the slowest thing in the build on a Lisp that compiles them from source (modus).

(defun utf8-octets (string)
  "STRING as UTF-8 octets."
  (let ((out (make-array (length string) :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((put (b) (vector-push-extend b out)))
      (loop for ch across string
            for c = (char-code ch)
            do (cond ((< c #x80) (put c))
                     ((< c #x800) (put (logior #xc0 (ash c -6))) (put (logior #x80 (logand c #x3f))))
                     ((< c #x10000) (put (logior #xe0 (ash c -12))) (put (logior #x80 (logand (ash c -6) #x3f)))
                      (put (logior #x80 (logand c #x3f))))
                     (t (put (logior #xf0 (ash c -18))) (put (logior #x80 (logand (ash c -12) #x3f)))
                        (put (logior #x80 (logand (ash c -6) #x3f))) (put (logior #x80 (logand c #x3f)))))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun utf8-string (octets &key (start 0) (end (length octets)))
  "OCTETS[START,END) as text; a malformed sequence reads as U+FFFD (as babel's :errorp nil had it).
   Into one string the size of the octets (text is never longer than its UTF-8), cut to length at
   the end: a string-output stream a character at a time made a megabyte (a picture's base64 in
   JSON) never finish on a modus worker."
  (let ((out (make-string (max 0 (- end start)))) (j 0) (i start))
    (flet ((emit (code) (setf (char out j) (or (ignore-errors (code-char code)) (code-char #xfffd))) (incf j)))
      (loop while (< i end)
            do (let* ((b (aref octets i))
                      (len (cond ((< b #x80) 1) ((= (logand b #xe0) #xc0) 2) ((= (logand b #xf0) #xe0) 3)
                                 ((= (logand b #xf8) #xf0) 4) (t 0))))
                 (if (or (zerop len) (> (+ i len) end)
                         (loop for k from 1 below len thereis (/= (logand (aref octets (+ i k)) #xc0) #x80)))
                     ;; one replacement for the longest broken start -- a lead byte and the continuation
                     ;; bytes it did get -- as babel (and the WHATWG decoder) have it
                     (progn (emit #xfffd)
                            (incf i)
                            (when (plusp len)
                              (loop repeat (1- len)
                                    while (and (< i end) (= (logand (aref octets i) #xc0) #x80))
                                    do (incf i))))
                     (let ((c (if (= len 1) b (logand b (ash #xff (- (1+ len)))))))
                       (loop for k from 1 below len do (setf c (logior (ash c 6) (logand (aref octets (+ i k)) #x3f))))
                       ;; NOT A CHARACTER, though well-formed: an overlong form (C0 AF is "/" -- the old way past
                       ;; a path check), a UTF-16 surrogate, or past U+10FFFF.  One replacement, as babel has it.
                       (when (or (< c (case len (1 0) (2 #x80) (3 #x800) (t #x10000)))
                                 (<= #xd800 c #xdfff) (> c #x10ffff))
                         (setf c #xfffd))
                       (emit c)
                       (incf i len))))))
    (if (= j (length out)) out (subseq out 0 j))))

(defun base64-string (octets)
  "OCTETS in base64 (RFC 4648, padded): what cl-base64 was here for."
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
        (n (length octets)))
    (with-output-to-string (o)
      (loop for i from 0 below n by 3
            do (let* ((b0 (aref octets i))
                      (b1 (if (< (+ i 1) n) (aref octets (+ i 1)) 0))
                      (b2 (if (< (+ i 2) n) (aref octets (+ i 2)) 0))
                      (w (logior (ash b0 16) (ash b1 8) b2)))
                 (write-char (char alphabet (ldb (byte 6 18) w)) o)
                 (write-char (char alphabet (ldb (byte 6 12) w)) o)
                 (write-char (if (< (+ i 1) n) (char alphabet (ldb (byte 6 6) w)) #\=) o)
                 (write-char (if (< (+ i 2) n) (char alphabet (ldb (byte 6 0) w)) #\=) o))))))

(defun %octets (x)
  (etypecase x
    (null nil)
    (string (utf8-octets x))
    ((vector (unsigned-byte 8)) x)))

(defun %text (octets) (utf8-string octets))

(defun %drain (stream cap)
  "Up to CAP characters of STREAM as a string, then close it."
  (unwind-protect
       (with-output-to-string (o)
         (loop with n = 0
               for line = (read-line stream nil nil)
               while (and line (< n cap))
               do (write-line line o) (incf n (1+ (length line)))))
    (ignore-errors (close stream))))

(defun request (method url &key content headers (read-timeout 30) want-stream max-bytes binary)
  "Perform METHOD against URL.  Returns the body as a string (plus the status and headers as
   further values), or with WANT-STREAM a character stream over the body that the caller must
   close.  Signals HTTP-REQUEST-FAILED on any non-2xx status.

   BINARY returns the body as a (VECTOR (UNSIGNED-BYTE 8)) instead of a decoded string — for
   payloads that are not text at all (an EHBP-sealed body, an attestation document).  The error
   body of a failed request is still decoded as text, since that is what a provider's refusal
   message is."
  (let ((body (%octets content)))
    (if want-stream
        (let* ((s (seal.http:open-request method url :headers headers :body body
                                                     :timeout read-timeout))
               (code (seal.http:body-stream-status s)))
          (if (<= 200 code 299)
              s
              (error 'http-request-failed :status code :url url
                                          :body (%drain s *error-body-cap*))))
        (let* ((r (seal.http:request method url :headers headers :body body
                                                :timeout read-timeout :max-body max-bytes))
               (code (seal.http:response-status r))
               (raw (seal.http:response-body r))
               (text (%text raw)))
          (if (<= 200 code 299)
              (values (if binary (coerce raw '(vector (unsigned-byte 8))) text)
                      code (seal.http:response-headers r))
              ;; A BINARY caller gets the raw octets in the condition too: its
              ;; error body may itself be an encrypted frame, and decoding it as
              ;; text would destroy the only copy of the bytes it needs.
              (error 'http-request-failed
                     :status code :url url
                     :body (if binary (coerce raw '(vector (unsigned-byte 8))) text)
                     :headers (seal.http:response-headers r)))))))

(defun get (url &rest keys &key headers read-timeout want-stream max-bytes binary)
  (declare (ignore headers read-timeout want-stream max-bytes binary))
  (apply #'request "GET" url keys))

(defun post (url &rest keys &key content headers read-timeout want-stream binary)
  (declare (ignore content headers read-timeout want-stream binary))
  (apply #'request "POST" url keys))
