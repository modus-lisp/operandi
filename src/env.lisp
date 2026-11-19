;;;; env.lisp — settings seeded from the environment, re-read at every start
;;;;
;;;; operandi runs from a saved core (bin/operandi), and a DEFVAR's initial
;;;; value is computed once, when the core is BUILT.  A setting seeded with
;;;; (uiop:getenv "OPERANDI_…") in its DEFVAR therefore froze whatever the
;;;; build's environment held: a core built while someone was testing with
;;;; OPERANDI_MAX_ITERS=2 capped every later run at two iterations, and setting
;;;; the variable at run time changed nothing.
;;;;
;;;; DEFENV defines the variable as before AND records a seed that re-evaluates
;;;; the same form.  RESTORE-ENVIRONMENT runs UIOP's restore hook and then those
;;;; seeds; every entry point calls it before parsing its flags, so the
;;;; environment of the RUN decides, and a flag still overrides it.  A plain
;;;; SAVE-LISP-AND-DIE core runs no restore hooks on its own (SBCL's
;;;; *INIT-HOOKS* is empty), which is also why UIOP's own — command-line
;;;; arguments, the standard streams, the temporary directory — never ran.

(defpackage #:operandi.env
  (:use #:cl)
  (:export #:defenv #:restore-environment #:cwd))

(in-package #:operandi.env)

(defvar *seeds* '()
  "Every DEFENV's re-seed, oldest first.  Kept here rather than only as UIOP image-restore hooks: modus's
   UIOP has no REGISTER-IMAGE-RESTORE-HOOK, and the seeds are operandi's to run either way.")

(defmacro defenv (name form &optional doc (definer 'defvar))
  "Define NAME with the value of FORM, like DEFVAR (or DEFINER), and re-evaluate
   FORM into NAME whenever the image is restored -- see RESTORE-ENVIRONMENT.
   FORM is the seed: it reads the environment and supplies the default."
  `(progn
     (,definer ,name ,form ,@(when doc (list doc)))
     (let ((seed (lambda () (setf ,name ,form))))
       (setf *seeds* (append (remove ',name *seeds* :key #'car) (list (cons ',name seed)))))
     ',name))

(defun restore-environment ()
  "Run the image-restore hooks: UIOP's own, and every DEFENV seed.  Call it
   once at startup, BEFORE any flag sets a DEFENV variable, or the flag's value
   is overwritten by the environment's."
  (when (fboundp 'uiop:call-image-restore-hook) (uiop:call-image-restore-hook))
  (dolist (s *seeds*) (funcall (cdr s))))

(defun cwd ()
  "The working directory: the OS's, else *DEFAULT-PATHNAME-DEFAULTS*, else home.  An image with no OS
   process directory to ask (modus on a phone: UIOP:GETCWD signals) still has a place to call the project."
  (or (ignore-errors (uiop:getcwd))
      (let ((d *default-pathname-defaults*)) (and d (plusp (length (namestring d))) d))
      (user-homedir-pathname)))
