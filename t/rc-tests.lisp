;;;; rc-tests.lisp — what counts as an inbound request in a mirrored Marmot chat.
;;;; Run with:
;;;;
;;;;   sbcl --non-interactive --load t/rc-tests.lisp
;;;;
;;;; The rule under test: in a DIRECT chat (just us and the other member) every
;;;; message from the other member drives a turn; in a group, a message must
;;;; mention this run's @op- token or our npub.  Our own "> prompt" mirrors are
;;;; never requests, in either case.

(push #p"/Users/vinnyfiano/workspace/modus-lisp/seal/" asdf:*central-registry*)
(asdf:load-system :operandi)

(defpackage #:operandi.tests.rc
  (:use #:cl)
  (:local-nicknames (#:rc #:operandi.rc)))
(in-package #:operandi.tests.rc)

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

(defun inbound (content &key direct npub)
  "Run the real %INBOUND-TEXT with a fresh mention token and DIRECT bound."
  (let ((rc::*mention* "@op-abc123")
        (rc::*direct-p* direct))
    (rc::%inbound-text content npub)))

(defvar *us* "npub1hj95rckmrq3qkdk6kgd7cdxfjzgyqtds0ny5rumzf9qa50ulumhq4dpzew")

(format t "~&rc inbound rules~%")

;;; --- direct chat: no mention needed -----------------------------------------

(check "direct: a bare message is a request"
  (equal "good morning" (inbound "good morning" :direct t)))

(check "direct: a message that happens to mention us is still one request"
  (equal "@op-abc123 hi" (inbound "@op-abc123 hi" :direct t)))

(check "direct: our own mirror is not a request"
  (null (inbound "> good morning

Good morning!" :direct t)))

(check "direct: a mirror with leading whitespace is not a request"
  (null (inbound "  > hi

answer" :direct t)))

(check "direct: a blank message is not a request"
  (null (inbound "   " :direct t)))

(check "direct: a non-string is not a request"
  (null (inbound nil :direct t)))

;;; --- group chat: the mention is still required ------------------------------

(check "group: an unmentioned message is not a request"
  (null (inbound "good morning" :direct nil)))

(check "group: the @op- token makes it a request"
  (equal "@op-abc123 good morning" (inbound "@op-abc123 good morning" :direct nil)))

(check "group: the token is matched case-insensitively"
  (equal "@OP-ABC123 hi" (inbound "@OP-ABC123 hi" :direct nil)))

(check "group: our npub makes it a request (WhiteNoise rewrites @token to an npub)"
  (equal (format nil "~A hi" *us*) (inbound (format nil "~A hi" *us*) :direct nil :npub *us*)))

(check "group: our own mirror is not a request even if it quotes the token"
  (null (inbound "> @op-abc123 hi

answer" :direct nil)))

;;; --- not mirroring at all ---------------------------------------------------

(check "no mirror: nothing is a request"
  (let ((rc::*mention* nil) (rc::*direct-p* t))
    (null (rc::%inbound-text "good morning" nil))))

;;; --- the direct-chat predicate ----------------------------------------------

(check "direct-p is NIL before any chat is opened"
  (null (rc:direct-p)))

;;; --- the handler itself ------------------------------------------------------
;;;
;;; These drive the real %ON-EVENT with cl-marmot's own event/message structs, so
;;; the wiring (not just the predicate) is covered.  *CLIENT* is NIL: the reaction
;;; and the npub lookup are best-effort and must not break delivery.

(funcall (read-from-string "ql:quickload") :cl-marmot :silent t)

(defun event (type &key message)
  (funcall (find-symbol "%MAKE-EVENT" "MARMOT") :type type :chat nil :message message))

(defun message (content &key own)
  (funcall (find-symbol "%MAKE-MESSAGE" "MARMOT") :content content :own-p own))

(defmacro with-rc ((&key direct) &body body)
  `(let ((rc::*mention* "@op-abc123")
         (rc::*direct-p* ,direct)
         (rc::*client* nil)
         (rc::*chat* nil)
         (rc::*on-inbound* nil)
         (rc::*pending-inbound* '()))
     ,@body))

(check "handler: a direct chat's unmentioned message is delivered"
  (with-rc (:direct t)
    (rc::%on-event (event :message :message (message "good morning")))
    (equal '("good morning") rc::*pending-inbound*)))

(check "handler: our own message is never delivered"
  (with-rc (:direct t)
    (rc::%on-event (event :message :message (message "> hi" :own t)))
    (null rc::*pending-inbound*)))

(check "handler: a group's unmentioned message is not delivered"
  (with-rc (:direct nil)
    (rc::%on-event (event :message :message (message "good morning")))
    (null rc::*pending-inbound*)))

(check "handler: a group's mentioned message is delivered"
  (with-rc (:direct nil)
    (rc::%on-event (event :message :message (message "@op-abc123 good morning")))
    (equal '("@op-abc123 good morning") rc::*pending-inbound*)))

(check "handler: a membership change re-decides whether the chat is direct"
  (with-rc (:direct t)
    (rc::%on-event (event :member-added))
    (null rc::*direct-p*)))

(format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
