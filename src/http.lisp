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
;;;; error), with babel, which is portable, rather than an implementation's own octet functions.

(defpackage #:operandi.http
  (:use #:cl)
  (:shadow #:get)
  (:export #:get #:post #:request
           #:http-request-failed #:response-status #:response-body #:response-url))

(in-package #:operandi.http)

(define-condition http-request-failed (error)
  ((status :initarg :status :reader response-status)
   (body   :initarg :body   :reader response-body)
   (url    :initarg :url    :reader response-url))
  (:report (lambda (c s)
             (let ((b (response-body c)))
               (format s "HTTP ~a from ~a~@[: ~a~]" (response-status c) (response-url c)
                       (and (stringp b) (plusp (length b))
                            (subseq b 0 (min 200 (length b)))))))))

(defparameter *error-body-cap* 65536
  "How much of a FAILED streamed response is read into the condition.  Error bodies are small;
   the cap only matters if a server answers a stream with a failure and then keeps talking.")

(defun %octets (x)
  (etypecase x
    (null nil)
    (string (babel:string-to-octets x :encoding :utf-8))
    ((vector (unsigned-byte 8)) x)))

(defun %text (octets)
  (babel:octets-to-string (coerce octets '(simple-array (unsigned-byte 8) (*)))
                          :encoding :utf-8 :errorp nil))

(defun %drain (stream cap)
  "Up to CAP characters of STREAM as a string, then close it."
  (unwind-protect
       (with-output-to-string (o)
         (loop with n = 0
               for line = (read-line stream nil nil)
               while (and line (< n cap))
               do (write-line line o) (incf n (1+ (length line)))))
    (ignore-errors (close stream))))

(defun request (method url &key content headers (read-timeout 30) want-stream max-bytes)
  "Perform METHOD against URL.  Returns the body as a string (plus the status and headers as
   further values), or with WANT-STREAM a character stream over the body that the caller must
   close.  Signals HTTP-REQUEST-FAILED on any non-2xx status."
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
               (text (%text (seal.http:response-body r))))
          (if (<= 200 code 299)
              (values text code (seal.http:response-headers r))
              (error 'http-request-failed :status code :body text :url url))))))

(defun get (url &rest keys &key headers read-timeout want-stream max-bytes)
  (declare (ignore headers read-timeout want-stream max-bytes))
  (apply #'request "GET" url keys))

(defun post (url &rest keys &key content headers read-timeout want-stream)
  (declare (ignore content headers read-timeout want-stream))
  (apply #'request "POST" url keys))
