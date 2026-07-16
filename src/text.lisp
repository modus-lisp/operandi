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
  (:export #:*result-budget* #:bound-result
           #:*offload-dir* #:offload-string #:save-and-bound))

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

;;; ------------------------- reversible offload -----------------------
;;; Bounding drops the middle from context — but disk is cheap, so keep the
;;; FULL result on disk and point the agent at it. Then a truncation is
;;; reversible: instead of re-running the tool (costly, and for a web fetch
;;; not even reproducible — the page changes, it re-triggers the sanitizer),
;;; the agent just Reads the saved file. Mirrors the engine's compaction
;;; offload, applied at tool-return time.

(defparameter *offload-dir*
  (namestring (merge-pathnames ".operandi/offload/" (user-homedir-pathname)))
  "Directory where SAVE-AND-BOUND writes the full copy of a large tool
   result. Recoverable with the Read tool.")

(defvar *offload-counter* 0
  "Monotonic suffix so concurrent offloads within a second don't collide.")

(defun offload-string (s &key (prefix "blob") (dir *offload-dir*))
  "Write string S verbatim to a fresh file under DIR; return its absolute
   namestring, or NIL on any I/O error (caller degrades to a plain bound
   result). The filename is prefix + time + counter — no randomness needed."
  (handler-case
      (progn
        (ensure-directories-exist dir)
        (let ((path (merge-pathnames
                     (format nil "~A-~D-~D.txt" prefix
                             (get-universal-time) (incf *offload-counter*))
                     dir)))
          (with-open-file (o path :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
            (write-string s o))
          (namestring (truename path))))
    (error () nil)))

(defun save-and-bound (s &key (budget *result-budget*) (head-frac 0.5d0)
                              (label "output") (prefix "blob") extra-hint)
  "Like BOUND-RESULT, but REVERSIBLE: when S is over budget, save the FULL S
   to a file and make the elision note point at it (Read it to recover the
   middle), so nothing is actually lost. Returns S unchanged when it fits;
   falls back to a plain bounded result (with EXTRA-HINT) if the offload
   fails. The saved copy is whatever the caller passes — pass the SANITIZED
   text, never raw untrusted bytes, so recovery stays safe."
  (if (<= (length s) budget)
      s
      (let ((path (offload-string s :prefix prefix)))
        (bound-result
         s :budget budget :head-frac head-frac :label label
         :hint (if path
                   (format nil "full ~A (~:D chars) saved to ~A — Read it (offset/limit) to see the rest~@[; ~A~]"
                           label (length s) path extra-hint)
                   (or extra-hint "re-run to see the rest"))))))
