;;; src/actor.lisp  (operandi.actor)
;;;
;;; The concurrency operandi uses: actors that share nothing and talk by
;;; messages.  One small API, two backends:
;;;
;;;   SBCL (and anything else with bordeaux-threads): an actor is a thread with
;;;     a mailbox (lock + condition variable + FIFO).  Messages go by reference.
;;;   modus: the hosted actor runtime (ACTORS-START & co. in CL-USER), M:N over
;;;     native threads, each actor with its own heap.  Messages are COPIED, a
;;;     function in one is refused, and an actor cannot store its own objects
;;;     into a global -- modus traps that store ("threads share no state").
;;;
;;; So write to the stricter one: a message is DATA (numbers, strings, symbols,
;;; lists, vectors, hash tables, structs), never a closure or a stream, and it
;;; is not mutated after it is sent.  An actor's state is its own local
;;; variables; anything two actors both need belongs to one of them and the
;;; other asks for it.
;;;
;;;   (spawn 'fn arg...)         start an actor running (FN ARG...); returns its id.
;;;                              FN is a SYMBOL (modus actors cannot receive a
;;;                              closure).  With :LINK T first, the spawner gets
;;;                              (:EXIT id reason) when it ends: reason :NORMAL, or
;;;                              (:ERROR "message").
;;;   (send id msg)              asynchronous; never blocks on the receiver.
;;;   (receive &optional secs)   next message: (values msg t), or (values nil nil)
;;;                              after SECS seconds (NIL = wait forever).
;;;   (receive-if pred &optional secs)
;;;                              first message satisfying PRED; the others stay
;;;                              queued in order.
;;;   (self)                     this actor's id (the main thread is an actor too).

(defpackage #:operandi.actor
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export #:spawn #:send #:receive #:receive-if #:self
           #:*actor-slots* #:*actor-region-bytes* #:*actor-threads*
           #:per-computation))

(in-package #:operandi.actor)

(defparameter *actor-threads* 4
  "modus: scheduler threads carrying the actors.  An actor blocked in a system
   call (a socket read, a subprocess) holds its thread, so leave headroom.")
(defparameter *actor-slots* 16 "modus: how many actors can be alive at once.")
(defparameter *actor-region-bytes* (* 16 1024 1024)
  "modus: each actor's heap (a semispace pair of this size).")

(defmacro per-computation (&rest symbols)
  "Declare SYMBOLS' values private to each thread/actor on modus (each starts
   with NIL); a no-op elsewhere, where they stay ordinary globals."
  (declare (ignorable symbols))
  #+modus
  `(let ((reg (find-symbol "REGISTER-PER-COMPUTATION-SPECIAL" "COMMON-LISP-USER")))
     (when (and reg (fboundp reg))
       (dolist (s ',symbols) (funcall reg s))))
  #-modus nil)

;;; ------------------------------------------------------------------
;;; the trampoline every actor starts in (both backends)
;;; ------------------------------------------------------------------

(defun %describe-condition (e)
  "A one-line account of E, with a TYPE-ERROR's datum and expected type (some
   implementations print a type error as just its class name)."
  (or (ignore-errors
       (if (typep e 'type-error)
           (format nil "~A: ~S is not of type ~S" (type-of e)
                   (type-error-datum e) (type-error-expected-type e))
           (format nil "~A: ~A" (type-of e) e)))
      (ignore-errors (princ-to-string (type-of e)))
      "an error"))

(defun %run (fn args parent link)
  "Run (FN . ARGS); report how it ended to PARENT when LINK."
  (let ((reason :normal))
    (handler-case (apply fn args)
      (serious-condition (e)
        (setf reason (list :error (%describe-condition e)))))
    (when link (send parent (list :exit (self) reason)))
    reason))

;;; ------------------------------------------------------------------
;;; modus: the hosted actor runtime
;;; ------------------------------------------------------------------

#+modus
(progn
  (defvar *started* nil "Set by the main thread when it starts the runtime.")

  (defun %ensure-runtime ()
    (unless *started*
      (cl-user::actors-start *actor-threads* *actor-slots* *actor-region-bytes*)
      (setf *started* t)))

  (defun %actor-entry ()
    "What every modus actor runs: its first message says what to do."
    (let ((init (cl-user::actors-receive)))
      (destructuring-bind (tag fn args parent link) init
        (declare (ignore tag))
        (%run fn args parent link))))

  (defun self ()
    ;; Before the runtime starts there is only the main thread, actor 1 (and
    ;; ACTORS-SELF has nothing to read yet).
    (if *started*
        (let ((id (cl-user::actors-self))) (if (and (integerp id) (plusp id)) id 1))
        1))

  (defun spawn (fn &rest args)
    (let ((link nil))
      (when (eq fn :link) (setf link (pop args) fn (pop args)))
      (check-type fn symbol)
      (%ensure-runtime)
      (let ((id (cl-user::actors-spawn '%actor-entry)))
        (cl-user::actors-send id (list :%init fn args (self) link))
        id)))

  (defun send (id msg) (cl-user::actors-send id msg) msg)

  (defun %ms (secs) (and secs (max 0 (round (* secs 1000)))))

  (defun receive (&optional secs) (cl-user::actors-receive (%ms secs)))

  (defun receive-if (pred &optional secs) (cl-user::actors-receive-if pred (%ms secs))))

;;; ------------------------------------------------------------------
;;; threads: a mailbox per thread
;;; ------------------------------------------------------------------

#-modus
(progn
  (defstruct (mbox (:constructor %make-mbox))
    (lock (bt:make-lock "operandi-mbox"))
    (cv (bt:make-condition-variable))
    (q nil) (tail nil))

  (defvar *self* nil "This thread's mailbox (bound in every actor thread).")
  (defvar *main* nil "The main thread's mailbox, made on first use.")
  (defvar *main-lock* (bt:make-lock "operandi-main-mbox"))

  (defun self ()
    (or *self*
        (bt:with-lock-held (*main-lock*) (or *main* (setf *main* (%make-mbox))))))

  (defun send (box msg)
    (bt:with-lock-held ((mbox-lock box))
      (let ((cell (list msg)))
        (if (mbox-tail box) (setf (cdr (mbox-tail box)) cell) (setf (mbox-q box) cell))
        (setf (mbox-tail box) cell))
      (bt:condition-notify (mbox-cv box)))
    msg)

  (defun %take (box pred)
    "Under BOX's lock: unlink and return the first message satisfying PRED."
    (let ((prev nil) (q (mbox-q box)))
      (loop while q do
        (when (or (null pred) (funcall pred (car q)))
          (if prev (setf (cdr prev) (cdr q)) (setf (mbox-q box) (cdr q)))
          (when (eq q (mbox-tail box)) (setf (mbox-tail box) prev))
          (return-from %take (values (car q) t)))
        (setf prev q q (cdr q)))
      (values nil nil)))

  (defun receive-if (pred &optional secs)
    (let* ((box (self))
           (deadline (and secs (+ (get-internal-real-time)
                                  (round (* secs internal-time-units-per-second))))))
      (bt:with-lock-held ((mbox-lock box))
        (loop
          (multiple-value-bind (m got) (%take box pred)
            (when got (return (values m t))))
          (let ((left (and deadline (/ (- deadline (get-internal-real-time))
                                       internal-time-units-per-second))))
            (when (and left (<= left 0)) (return (values nil nil)))
            (bt:condition-wait (mbox-cv box) (mbox-lock box) :timeout left))))))

  (defun receive (&optional secs) (receive-if nil secs))

  (defun spawn (fn &rest args)
    (let ((link nil))
      (when (eq fn :link) (setf link (pop args) fn (pop args)))
      (check-type fn symbol)
      (let ((box (%make-mbox)) (parent (self)))
        (bt:make-thread (lambda () (let ((*self* box)) (%run fn args parent link)))
                        :name (format nil "actor ~(~A~)" fn))
        box))))
