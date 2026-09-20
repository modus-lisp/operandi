;;; src/linedit-ext.lisp
;;;
;;; Multi-line editing for linedit. Loaded AFTER linedit (bin/operandi bakes
;;; both into the core; the TUI loads this lazily otherwise), so it can name
;;; linedit's internals directly — it isn't part of the operandi system.
;;;
;;; Stock linedit lets ^J insert a newline into the buffer, and then falls
;;; apart: its redisplay lays the buffer out as one flat string wrapped at
;;; the terminal width (row = index / columns), so every cursor move after a
;;; newline is off by a row, the screen garbles, and the error drops the TUI
;;; to its plain read-line fallback. Two changes:
;;;
;;;   - DISPLAY for the smart terminal computes rows and columns by walking
;;;     the string, honoring both wrapping and embedded newlines.
;;;   - Up/Down move within the buffer when there is a line to move to, and
;;;     fall back to history only from the first/last line.
;;;
;;;   - ^J inserts a newline. Stock linedit translates BOTH LF and CR to
;;;     "Return" (submit), so ^J only looked like a newline: linedit finished
;;;     the line and the agent got the first line as its message. Enter (CR
;;;     — linedit's raw mode clears icrnl) still submits. A pasted block now
;;;     lands as one multi-line message instead of one message per line.

(defpackage #:operandi.linedit-ext
  (:use #:cl)
  (:export #:install))
(in-package #:operandi.linedit-ext)

(defun layout (string n columns)
  "Where index N of STRING lands on screen: (values row col wrapped), row
   1-based like linedit's FIND-ROW. WRAPPED is T when the char before N
   filled the last column — the terminal is then still on the previous
   row with the wrap pending, which matters for what a newline or the
   final cursor fix-up should do."
  (let ((row 1) (col 0) (wrapped nil))
    (loop for i below (min n (length string))
          for c = (char string i)
          do (cond ((char= c #\Newline)
                    ;; in the pending-wrap state the terminal's newline just
                    ;; realizes the row we already counted
                    (unless wrapped (incf row))
                    (setf col 0 wrapped nil))
                   ((>= (incf col) columns)
                    (incf row)
                    (setf col 0 wrapped t))
                   (t (setf wrapped nil))))
    (values row col wrapped)))

(defmethod linedit::display ((backend linedit::smart-terminal) &key prompt line point markup)
  (let* (#+(or sbcl cmu) (*terminal-io* *standard-output*)
         (columns (linedit::backend-columns backend))
         (old-markup (linedit::old-markup backend))
         (old-point (linedit::old-point backend))
         (old (linedit::old-string backend))
         (new (linedit::concat prompt line))
         (end (length new)))
    (multiple-value-bind (old-row old-col) (layout old old-point columns)
      (when (linedit::dirty-p backend)
        (setf old-markup 0 old-point 0 old-col 0 old-row 1))
      (multiple-value-bind (marked-line markup)
          (if markup
              (linedit::dwim-mark-parens line point
                                         :pre-mark (linedit::paren-style)
                                         :post-mark terminfo:exit-attribute-mode)
              (values line point))
        (let* ((full (linedit::concat prompt marked-line))
               (point (+ point (length prompt)))
               (diff (mismatch new old))
               ;; old and new agree up to START, so its screen position is
               ;; the same on either string
               (start (linedit::min* old-point point markup old-markup diff end)))
          (multiple-value-bind (rows end-col end-wrapped) (layout new end columns)
            (multiple-value-bind (point-row point-col) (layout new point columns)
              (multiple-value-bind (start-row start-col) (layout new start columns)
                (linedit::move-in-column :col start-col
                                         :vertical (- old-row start-row)
                                         :clear-to-eos t
                                         :current-col old-col)
                (write-string (subseq full start))
                ;; linedit's fix-wraparound, minus the false positive: a
                ;; buffer ending in a newline also has col 0, but the
                ;; terminal already moved down for it
                (when (and (< start end) end-wrapped)
                  (terminfo:tputs terminfo:cursor-down))
                (linedit::move-in-column :col point-col
                                         :vertical (- rows point-row)
                                         :current-col end-col)
                (setf (linedit::old-string backend) new
                      (linedit::old-markup backend) markup
                      (linedit::old-point backend) point
                      (linedit::dirty-p backend) nil)))))))
    (force-output *terminal-io*)))

(defun line-start (string point)
  "Index where the line containing POINT begins."
  (let ((nl (position #\Newline string :end point :from-end t)))
    (if nl (1+ nl) 0)))

(defun line-end (string point)
  "Index of the newline ending the line containing POINT, or the length."
  (or (position #\Newline string :start point) (length string)))

(defvar *goal-col* 0
  "Column to aim for on consecutive vertical moves — so Up, Up through a
   blank line still lands where the first Up was aimed, as in Emacs.")
(defvar *goal-point* -1
  "Point right after the last vertical move; any other point means the
   user did something in between and the goal is reset to the real column.")

(defun goal-column (string point)
  (let ((col (- point (line-start string point))))
    (if (= point *goal-point*) *goal-col* (setf *goal-col* col))))

(defun move-to-line (editor string bol goal)
  "Put point at column GOAL of the line starting at BOL, clamped to its end."
  (setf *goal-point*
        (setf (linedit::get-point editor) (min (+ bol goal) (line-end string bol)))))

(defun up-or-history (chord editor)
  "Up: previous line of the buffer at the goal column; history from the
   first line."
  (let* ((s (linedit::get-string editor))
         (p (linedit::get-point editor))
         (bol (line-start s p)))
    (if (zerop bol)
        (linedit::history-previous chord editor)
        (let ((goal (goal-column s p)))
          (move-to-line editor s (line-start s (1- bol)) goal)))))

(defun down-or-history (chord editor)
  "Down: next line of the buffer at the goal column; history from the
   last line."
  (let* ((s (linedit::get-string editor))
         (p (linedit::get-point editor))
         (eol (line-end s p)))
    (if (= eol (length s))
        (linedit::history-next chord editor)
        (let ((goal (goal-column s p)))
          (move-to-line editor s (1+ eol) goal)))))

(defun insert-newline (chord editor)
  (declare (ignore chord))
  (linedit::add-char #\Newline editor))

(defun install ()
  (setf (gethash "Up-arrow" linedit::*commands*) 'up-or-history
        (gethash "Down-arrow" linedit::*commands*) 'down-or-history
        (gethash "C-J" linedit::*commands*) 'insert-newline
        ;; LF was "Return"; CR (13) keeps that translation
        (gethash 10 linedit::*terminal-translations*) "C-J")
  t)

(install)
