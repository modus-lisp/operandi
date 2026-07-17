;;; src/text.lisp  (operandi.text)
;;;
;;; Output-shaping helpers shared by the tools. The one that matters is
;;; BOUND-RESULT: the Read tool's head+tail+elision envelope, generalised
;;; for any already-in-memory tool result.
;;;
;;; The point (agent UX): when a tool result is too big to hand back whole,
;;; DON'T keep a blind head-only prefix — that throws away the *verdict*,
;;; which for a command lives at the END (test failures, a stack trace, the
;;; exit line; an Eval's return value; a page's conclusion). Keep both ends
;;; with a note naming how much of the middle was dropped, and — crucially —
;;; a hint saying how to recover the rest. An elision the agent can act on
;;; is context; a silent cut is a trap.

(defpackage #:operandi.text
  (:use #:cl)
  (:export #:*result-budget* #:bound-result))

(in-package #:operandi.text)

(defparameter *result-budget* 50000
  "Default character budget for a bounded tool result. Over this, callers
   keep a head + tail with a self-describing elision note instead of a
   head-only cut.")

(defun %snap-forward (s idx &optional (window 200))
  "Index just past the first newline at/after IDX within WINDOW, so a head
   cut ends on a line boundary. Falls back to IDX when no newline is near."
  (let* ((len (length s))
         (idx (min (max idx 0) len))
         (nl (position #\Newline s :start idx :end (min len (+ idx window)))))
    (if nl (1+ nl) idx)))

(defun %snap-back (s idx &optional (window 200))
  "Index just past the last newline at/before IDX within WINDOW, so a tail
   slice starts on a line boundary. Falls back to IDX when no newline is near."
  (let* ((idx (min (max idx 0) (length s)))
         (nl (position #\Newline s :end idx :start (max 0 (- idx window))
                                   :from-end t)))
    (if nl (1+ nl) idx)))

(defun bound-result (s &key (budget *result-budget*) (head-frac 0.5d0)
                            (label "output") hint)
  "Bound string S to ~BUDGET chars, KEEPING BOTH ENDS: a head (HEAD-FRAC of
   the budget) and a tail (the remainder), separated by a note naming how
   many chars of the middle were elided. Returns S unchanged when it fits.

   HEAD-FRAC tilts the split — lower it to favour the tail for output whose
   answer is at the end (a command's verdict, a stack trace). LABEL names
   the thing in the note ('output', 'stdout', 'page'). HINT, if given, is a
   recovery suggestion appended to the note — say how to see the rest."
  (let ((len (length s)))
    (cond
      ((not (stringp s)) s)
      ((<= len budget) s)
      (t
       (let* ((head-len (max 0 (floor (* budget (max 0d0 (min 1d0 head-frac))))))
              (tail-len (max 0 (- budget head-len)))
              (head-end (%snap-forward s head-len))
              (tail-start (%snap-back s (- len tail-len)))
              (elided (- tail-start head-end)))
         (if (<= elided 0)
             ;; snapping collapsed the middle (budget ~ len) — not worth it.
             s
             (with-output-to-string (o)
               (write-string s o :start 0 :end head-end)
               (format o "~&~%… ~:D of ~:D chars of ~A elided~@[ — ~A~] …~%~%"
                       elided len label hint)
               (write-string s o :start tail-start))))))))
