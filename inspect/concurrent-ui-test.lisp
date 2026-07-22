;;; inspect/concurrent-ui-test.lisp
;;;
;;; Oracle for the concurrent raw-mode TUI internals (operandi.tui) — the parts
;;; that can be checked WITHOUT a real terminal or a live model:
;;;   - key decoding (control chars + CSI escape sequences),
;;;   - the raw-mode line editor (insert/backspace/delete/move/home/end/kill/history),
;;;   - the worker + queue: a submit while a turn is "running" is held and run as
;;;     the NEXT turn (submit-as-next-message), in order.
;;; The actual terminal escape rendering is exercised by hand on a pty; here we
;;; assert the state machine.
;;;
;;; Exit 0 iff all checks pass; joins fitness as a swarm/fitness oracle.

(require :asdf)
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:concurrent-ui-test (:use #:cl)
  (:local-nicknames (#:tui #:operandi.tui) (#:session #:operandi.session) (#:bt #:bordeaux-threads)))
(in-package #:concurrent-ui-test)

(defvar *fails* 0)
(defparameter *out* *standard-output*)   ; real stdout, even under with-sink
(defmacro check (name form)
  `(handler-case (if ,form (format *out* "  ok   ~A~%" ,name)
                     (progn (incf *fails*) (format *out* "  FAIL ~A~%" ,name)))
     (error (e) (incf *fails*) (format *out* "  ERR  ~A: ~A~%" ,name e))))

(defun key-of (bytes)
  "Run tui::read-key against BYTES (a string of raw input)."
  (with-input-from-string (s bytes)
    (let ((*standard-input* s)) (tui::read-key))))

(format t "~&== key decoding ==~%")
(check "printable char"   (eql #\a (key-of "a")))
(check "Enter (CR)"       (eq :enter (key-of (string #\Return))))
(check "Enter (LF)"       (eq :enter (key-of (string #\Newline))))
(check "Backspace 0x7f"   (eq :backspace (key-of (string (code-char 127)))))
(check "Ctrl-C"           (eq :ctrl-c (key-of (string (code-char 3)))))
(check "Ctrl-D"           (eq :ctrl-d (key-of (string (code-char 4)))))
(check "Ctrl-A -> home"   (eq :home (key-of (string (code-char 1)))))
(check "Ctrl-E -> end"    (eq :end (key-of (string (code-char 5)))))
(check "arrow up"         (eq :up    (key-of (format nil "~C[A" #\Escape))))
(check "arrow down"       (eq :down  (key-of (format nil "~C[B" #\Escape))))
(check "arrow right"      (eq :right (key-of (format nil "~C[C" #\Escape))))
(check "arrow left"       (eq :left  (key-of (format nil "~C[D" #\Escape))))
(check "home CSI"         (eq :home  (key-of (format nil "~C[H" #\Escape))))
(check "end CSI"          (eq :end   (key-of (format nil "~C[F" #\Escape))))
(check "delete CSI 3~"    (eq :delete (key-of (format nil "~C[3~~" #\Escape))))

(format t "~&== line editor ==~%")
;; editor calls ui-repaint -> paint-input -> raw-write; swallow that output.
(setf (symbol-value (find-symbol "*UI-SESS*" "OPERANDI.TUI")) (session::make-session))
(setf (symbol-value (find-symbol "*ROWS*" "OPERANDI.TUI")) 40
      (symbol-value (find-symbol "*COLS*" "OPERANDI.TUI")) 100)
(defmacro with-sink (&body body)
  `(let ((*standard-output* (make-broadcast-stream))) ,@body))
(macrolet ((buf () `(symbol-value (find-symbol "*BUF*" "OPERANDI.TUI")))
           (cur () `(symbol-value (find-symbol "*CUR*" "OPERANDI.TUI"))))
  (with-sink
    (tui::ui-clear-input)
    (map nil #'tui::ui-insert "hello")
    (check "insert builds buffer" (and (string= (buf) "hello") (= (cur) 5)))
    (tui::ui-backspace)
    (check "backspace" (and (string= (buf) "hell") (= (cur) 4)))
    (tui::ui-home)
    (check "home moves caret to 0" (= (cur) 0))
    (tui::ui-insert #\X)
    (check "insert at caret" (and (string= (buf) "Xhell") (= (cur) 1)))
    (tui::ui-end)
    (check "end moves caret to len" (= (cur) 5))
    (tui::ui-move -2)
    (check "move left" (= (cur) 3))
    (tui::ui-delete)
    (check "delete at caret" (string= (buf) "Xhel"))
    (tui::ui-kill-eol)
    (check "kill to eol" (string= (buf) "Xhe"))
    (tui::ui-kill)
    (check "kill line" (and (string= (buf) "") (= (cur) 0)))
    ;; history: submit two lines, then arrow-up recalls the newest
    (setf (symbol-value (find-symbol "*HISTORY*" "OPERANDI.TUI")) (list "first" "second")
          (symbol-value (find-symbol "*HIST-IDX*" "OPERANDI.TUI")) nil)
    (tui::ui-history -1)
    (check "history up recalls newest" (string= (buf) "second"))
    (tui::ui-history -1)
    (check "history up again -> older" (string= (buf) "first"))
    (tui::ui-history 1)
    (check "history down -> newer" (string= (buf) "second"))))

(format t "~&== worker + queue: submit-as-next-message, in order ==~%")
;; Stub run-turn to record the prompt and simulate work; drive the real worker.
(let* ((recorded '())
       (rec-lock (bt:make-lock))
       (orig (symbol-function (find-symbol "RUN-TURN" "OPERANDI.TUI"))))
  (setf (symbol-function (find-symbol "RUN-TURN" "OPERANDI.TUI"))
        (lambda (sess prompt)
          (declare (ignore sess))
          (bt:with-lock-held (rec-lock) (push prompt recorded))
          (sleep 0.3)))                     ; simulate a turn taking time
  (unwind-protect
      (with-sink
        (let ((sess (session::make-session)))
          ;; init the worker globals the way repl-concurrent does
          (setf (symbol-value (find-symbol "*UI-SESS*" "OPERANDI.TUI")) sess
                (symbol-value (find-symbol "*QUEUE*" "OPERANDI.TUI")) nil
                (symbol-value (find-symbol "*TURN-ACTIVE*" "OPERANDI.TUI")) nil
                (symbol-value (find-symbol "*QUIT*" "OPERANDI.TUI")) nil
                (symbol-value (find-symbol "*IDLE-SAVED*" "OPERANDI.TUI")) nil
                (symbol-value (find-symbol "*LOG-BOL*" "OPERANDI.TUI")) t
                (symbol-value (find-symbol "*BUF*" "OPERANDI.TUI")) ""
                (symbol-value (find-symbol "*CUR*" "OPERANDI.TUI")) 0
                (symbol-value (find-symbol "*UI-OUT*" "OPERANDI.TUI"))
                (symbol-function (find-symbol "UI-WRITE-CONCURRENT" "OPERANDI.TUI")))
          (let ((w (bt:make-thread (lambda () (tui::worker-loop sess)) :name "test-worker")))
            (setf (symbol-value (find-symbol "*WORKER*" "OPERANDI.TUI")) w)
            ;; submit "alpha" while idle -> starts running
            (setf (symbol-value (find-symbol "*BUF*" "OPERANDI.TUI")) "alpha")
            (tui::ui-submit sess)
            (sleep 0.1)
            (check "turn goes active on submit"
                   (symbol-value (find-symbol "*TURN-ACTIVE*" "OPERANDI.TUI")))
            ;; submit "beta" WHILE alpha is running -> must queue, run next
            (setf (symbol-value (find-symbol "*BUF*" "OPERANDI.TUI")) "beta")
            (tui::ui-submit sess)
            (check "beta is queued while busy"
                   (equal '("beta") (symbol-value (find-symbol "*QUEUE*" "OPERANDI.TUI"))))
            ;; wait for both to drain
            (loop repeat 40
                  until (bt:with-lock-held (rec-lock) (= 2 (length recorded)))
                  do (sleep 0.1))
            ;; stop the worker
            (setf (symbol-value (find-symbol "*QUIT*" "OPERANDI.TUI")) t)
            (bt:with-lock-held ((symbol-value (find-symbol "*TURN-LOCK*" "OPERANDI.TUI")))
              (bt:condition-notify (symbol-value (find-symbol "*TURN-CV*" "OPERANDI.TUI"))))
            (ignore-errors (bt:join-thread w))
            (check "both turns ran, in submit order"
                   (equal '("alpha" "beta") (reverse (bt:with-lock-held (rec-lock) recorded)))))))
    (setf (symbol-function (find-symbol "RUN-TURN" "OPERANDI.TUI")) orig)))

(format t "~&~%concurrent-ui-test: ~A failure~:P~%" *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
