;;;; env.lisp — settings seeded from the environment, re-read at every start
;;;;
;;;; operandi runs from a saved core (bin/operandi), and a DEFVAR's initial
;;;; value is computed once, when the core is BUILT.  A setting seeded with
;;;; (uiop:getenv "OPERANDI_…") in its DEFVAR therefore froze whatever the
;;;; build's environment held: a core built while someone was testing with
;;;; OPERANDI_MAX_ITERS=2 capped every later run at two iterations, and setting
;;;; the variable at run time changed nothing.
;;;;
;;;; DEFENV defines the variable as before AND registers a UIOP image-restore
;;;; hook that re-evaluates the same seed form.  RESTORE-ENVIRONMENT runs those
;;;; hooks; every entry point calls it before parsing its flags, so the
;;;; environment of the RUN decides, and a flag still overrides it.  A plain
;;;; SAVE-LISP-AND-DIE core runs no restore hooks on its own (SBCL's
;;;; *INIT-HOOKS* is empty), which is also why UIOP's own — command-line
;;;; arguments, the standard streams, the temporary directory — never ran.

(defpackage #:operandi.env
  (:use #:cl)
  (:export #:defenv #:restore-environment))

(in-package #:operandi.env)

(defmacro defenv (name form &optional doc (definer 'defvar))
  "Define NAME with the value of FORM, like DEFVAR (or DEFINER), and re-evaluate
   FORM into NAME whenever the image is restored -- see RESTORE-ENVIRONMENT.
   FORM is the seed: it reads the environment and supplies the default."
  `(progn
     (,definer ,name ,form ,@(when doc (list doc)))
     (uiop:register-image-restore-hook (lambda () (setf ,name ,form)) nil)
     ',name))

(defun restore-environment ()
  "Run the image-restore hooks: UIOP's own, and every DEFENV seed.  Call it
   once at startup, BEFORE any flag sets a DEFENV variable, or the flag's value
   is overwritten by the environment's."
  (uiop:call-image-restore-hook))
