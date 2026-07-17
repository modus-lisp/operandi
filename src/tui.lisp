;;; src/tui.lisp  (operandi.tui)
;;;
;;; An enhanced inline REPL for operandi — a "capable TUI" that scrolls
;;; like a normal terminal session (no alt-screen, no raw mode) but adds:
;;;   - live token streaming of the assistant's reply,
;;;   - coloured, self-describing tool-call lines (via operandi.hooks),
;;;   - multi-turn conversation memory (history threaded across turns),
;;;   - a per-turn metrics line + a running session cost in the prompt,
;;;   - slash commands (/help /clear /cost /model /system /tools /quit),
;;;   - Ctrl-C that aborts the *current turn* and drops back to the prompt
;;;     instead of killing the session.
;;;
;;; It is deliberately line-based: the engine already streams tokens
;;; through OPERANDI.ENGINE:*ON-TOKEN* and fires OPERANDI.HOOKS around every
;;; tool call, so the TUI just writes those events to *STANDARD-OUTPUT* as
;;; they happen and lets the terminal scroll. No threads, no termios — it
;;; works over ssh, in a pipe (colour auto-disables when stdout isn't a
;;; tty), and inside a bare `sbcl --load`.
;;;
;;; Entry point: (operandi.tui:repl).  bin/operandi.lisp dispatches the
;;; `tui` (and `shell`) subcommands here.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (ignore-errors (require :sb-posix)))

(defpackage #:operandi.tui
  (:use #:cl)
  (:local-nicknames (#:eng   #:operandi.engine)
                    (#:llm   #:operandi.llm)
                    (#:hooks #:operandi.hooks)
                    (#:tools #:operandi.tools))
  (:export #:repl #:*color*))

(in-package #:operandi.tui)

;;; ------------------------------ colour ------------------------------

(defvar *color* :auto
  "T = always colour, NIL = never, :AUTO = colour iff stdout is a tty and
   NO_COLOR is unset.")

(defun fd-tty-p (fd stream)
  "T when file-descriptor FD is a terminal. Prefers a real isatty (resolved
   at runtime via FIND-SYMBOL, since which one exists varies by SBCL build)
   and treats its verdict as authoritative — so a pipe reads as NOT a tty.
   Only when no isatty is available at all does it guess via
   INTERACTIVE-STREAM-P (which returns true even for pipes here)."
  (flet ((fbound (sym pkg)
           (let ((s (find-symbol sym pkg))) (and s (fboundp s) s))))
    (let ((f (or (fbound "ISATTY" "SB-POSIX")        ; boolean T/NIL
                 (fbound "UNIX-ISATTY" "SB-UNIX"))))  ; integer 1/0
      (if f
          (let ((r (ignore-errors (funcall f fd))))
            (if (integerp r) (plusp r) (and r t)))    ; normalise 0/1 vs T/NIL
          (ignore-errors (interactive-stream-p stream))))))

(defun stdout-tty-p () (fd-tty-p 1 *standard-output*))
(defun stdin-tty-p  () (fd-tty-p 0 *standard-input*))

(defun color-on-p ()
  (case *color*
    ((t) t)
    ((nil) nil)
    (t (and (not (uiop:getenv "NO_COLOR")) (stdout-tty-p)))))

(defparameter +sgr+
  '(:reset 0 :bold 1 :dim 2 :italic 3 :underline 4
    :red 31 :green 32 :yellow 33 :blue 34 :magenta 35 :cyan 36 :white 37
    :gray 90 :br-red 91 :br-green 92 :br-yellow 93 :br-cyan 96))

(defun paint (str &rest codes)
  "Wrap STR in the SGR attributes named by CODES (keywords), resetting
   after. Returns STR unchanged when colour is off."
  (if (and codes (color-on-p))
      (format nil "~C[~{~A~^;~}m~A~C[0m"
              #\Escape (mapcar (lambda (k) (getf +sgr+ k)) codes) str #\Escape)
      str))

;;; ------------------------- line-state tracking ----------------------
;;; Streaming tokens and tool lines share *STANDARD-OUTPUT*; we track
;;; whether the cursor is at the start of a line so a tool line (or the
;;; next assistant segment) can guarantee it starts fresh without emitting
;;; blank lines.

(defvar *at-bol* t "T when the cursor is column 0.")
(defvar *need-bullet* nil "T when the next streamed token should open a new
  assistant segment with a bullet (set at turn start and after each tool).")
(defvar *streamed* nil "String stream capturing what actually streamed this
  turn, so we can tell if the final answer still needs printing.")

(defun emit (str)
  "Write STR to stdout, updating *AT-BOL* from its last character."
  (when (and str (plusp (length str)))
    (write-string str)
    (setf *at-bol* (char= (char str (1- (length str))) #\Newline))))

(defun fresh ()
  "Ensure the cursor is at column 0 (emit a newline only if it isn't)."
  (unless *at-bol* (emit (string #\Newline))))

;;; ------------------------------ tool lines --------------------------

(defparameter +arg-key+
  '(("Read" . "file_path") ("Write" . "file_path") ("Edit" . "file_path")
    ("Bash" . "command")   ("Grep" . "pattern")     ("Glob" . "pattern")
    ("WebFetch" . "url")    ("WebSearch" . "query")  ("Remember" . "content"))
  "The single argument worth showing inline, per tool.")

(defun oneline (s max)
  "Collapse whitespace in S to single spaces and truncate to MAX chars
   with an ellipsis. Never errors on non-strings."
  (let* ((s (typecase s
              (string s)
              (null "")
              (t (princ-to-string s))))
         (flat (with-output-to-string (o)
                 (let ((sp nil))
                   (loop for c across s do
                     (cond ((member c '(#\Space #\Tab #\Newline #\Return))
                            (unless sp (write-char #\Space o) (setf sp t)))
                           (t (write-char c o) (setf sp nil))))))))
    (setf flat (string-trim " " flat))
    (if (> (length flat) max)
        (concatenate 'string (subseq flat 0 (max 0 (1- max))) "…")
        flat)))

(defun first-hash-value (args)
  (when (hash-table-p args)
    (loop for v being the hash-values of args do (return v))))

(defun tool-arg-summary (name args)
  (let* ((key (cdr (assoc name +arg-key+ :test #'string=)))
         (v (if (and key (hash-table-p args)) (gethash key args)
                (first-hash-value args))))
    (oneline v 64)))

(defun error-result-p (result error)
  (or error
      (and (stringp result)
           (or (eql 0 (search "TOOL ERROR" result))
               (eql 0 (search "REFUSED" result))))))

(defun tui-pre-hook (name args)
  (fresh)
  (emit (format nil "~A ~A~A~A~%"
                (paint "⏺" :cyan)
                (paint name :bold)
                (paint "(" :gray)
                (let ((s (tool-arg-summary name args)))
                  (format nil "~A~A" (paint s :gray) (paint ")" :gray)))))
  ;; text that follows the tool result belongs to a new assistant segment
  (setf *need-bullet* t))

(defun tui-post-hook (name args result error ms)
  (declare (ignore args))
  (fresh)
  (let* ((err (error-result-p result error))
         (mark (if err (paint "✗" :br-red) (paint "✔" :br-green)))
         (dur  (paint (format nil "~Ams" ms) :gray))
         (prev (oneline result 72)))
    (emit (format nil "  ~A ~A  ~A~%"
                  mark dur
                  (if err (paint prev :red) (paint prev :gray))))))

;;; ------------------------------ streaming ---------------------------

(defun on-token (tok)
  "OPERANDI.ENGINE:*ON-TOKEN* callback: stream TOK live and capture it."
  (when (and tok (plusp (length tok)))
    (when *need-bullet*
      (fresh)
      (emit (paint "⏺ " :br-cyan))
      (setf *need-bullet* nil))
    (write-string tok *streamed*)
    (emit tok)))

;;; ------------------------------ session -----------------------------

(defstruct session
  (history nil)                 ; message list threaded back into eng:run
  (usage (llm:make-usage))      ; cumulative across the whole session
  (turns 0))

(defun user-msg (text) (llm:ht "role" "user" "content" text))

(defun model-label ()
  (case llm:*llm-backend*
    (:openrouter (or llm:*llm-model* "openrouter"))
    (t "llama.cpp")))

(defun run-turn (sess prompt)
  "Run one agent turn on PROMPT, streaming to the terminal, threading
   SESS's history and folding usage. Ctrl-C aborts just this turn."
  (let* ((hist (session-history sess))
         (messages (if hist (append hist (list (user-msg prompt))) nil))
         (*at-bol* t)
         (*need-bullet* t)
         (*streamed* (make-string-output-stream))
         ;; add our renderers alongside whatever hooks are already installed
         ;; (e.g. the SQLite logger); scoped to this turn.
         (hooks:*pre-tool-hooks*  (cons #'tui-pre-hook hooks:*pre-tool-hooks*))
         (hooks:*post-tool-hooks* (cons #'tui-post-hook hooks:*post-tool-hooks*))
         (eng:*stream* t)
         (eng:*on-token* #'on-token))
    (handler-case
        (multiple-value-bind (text hist* iters usage)
            (eng:run prompt :history messages :verbose nil)
          (setf (session-history sess) hist*)
          (incf (session-turns sess))
          (llm:usage-incf (session-usage sess) usage)
          ;; If nothing streamed (non-streaming turn, or a salvaged/synthetic
          ;; answer), or the final text diverged from the stream, print it.
          (let ((streamed (string-right-trim '(#\Space #\Newline)
                                             (get-output-stream-string *streamed*)))
                (final (string-right-trim '(#\Space #\Newline) (or text ""))))
            (when (and (plusp (length final)) (string/= final streamed))
              (fresh)
              (emit (paint "⏺ " :br-cyan))
              (emit final)))
          (fresh)
          (emit (paint (format nil "  ~A iter · ~A~%" iters (llm:usage-summary usage))
                       :gray))
          (force-output))
      (#+sbcl sb-sys:interactive-interrupt #-sbcl error ()
        ;; The engine's in-flight state is discarded; the conversation
        ;; history is left as it was before this turn, so the next prompt
        ;; starts clean.
        (fresh)
        (emit (paint "  ⎋ interrupted — turn abandoned, session kept." :yellow))
        (emit (string #\Newline))
        (force-output)))))

;;; ------------------------------ commands ----------------------------

(defun cmd-help ()
  (format t "~&~A~%" (paint "commands:" :bold))
  (dolist (row '(("/help"          "show this help")
                 ("/clear"         "forget the conversation, start fresh")
                 ("/cost"          "session cost + token totals")
                 ("/model [id]"    "show or switch model (id like vendor/name → OpenRouter)")
                 ("/system"        "print the active system prompt")
                 ("/tools"         "list the tools the agent can call")
                 ("/quit  /exit"   "leave (Ctrl-D also works)")))
    (format t "  ~A  ~A~%"
            (paint (format nil "~12A" (first row)) :cyan) (second row)))
  (format t "~&Type anything else to hand it to the agent. Ctrl-C aborts a running turn.~%"))

(defun cmd-cost (sess)
  (let ((u (session-usage sess)))
    (format t "~&~A over ~A turn~:P — ~A~%"
            (paint "session" :bold) (session-turns sess) (llm:usage-summary u))))

(defun cmd-model (arg)
  (cond
    ((null arg)
     (format t "~&model: ~A  (backend ~A)~%"
             (paint (model-label) :cyan) llm:*llm-backend*))
    ((find #\/ arg)                       ; vendor/model → OpenRouter
     (llm:use-openrouter :model arg)
     (format t "~&→ OpenRouter ~A~%" (paint arg :cyan)))
    ((string-equal arg "llama")
     (llm:use-llama)
     (format t "~&→ local llama.cpp~%"))
    (t (format t "~&unknown model spec ~S — give a 'vendor/name' id or 'llama'.~%" arg))))

(defun cmd-system (sess)
  (declare (ignore sess))
  (format t "~&~A~%~A~%"
          (paint "system prompt:" :bold)
          (funcall (find-symbol "BUILD-SYSTEM-PROMPT" "OPERANDI.ENGINE"))))

(defun cmd-tools ()
  (format t "~&~A~%" (paint "tools:" :bold))
  (dolist (name (sort (copy-list (tools:default-tools)) #'string<))
    (let ((tool (tools:tool-by-name name)))
      (format t "  ~A  ~A~%"
              (paint (format nil "~12A" name) :cyan)
              (oneline (and tool (getf tool :description)) 68)))))

(defun handle-command (line sess)
  "Dispatch a /command. Returns :QUIT to end the REPL, T if handled, NIL
   if LINE isn't a command."
  (let* ((trimmed (string-trim " " line))
         (sp (position #\Space trimmed))
         (verb (subseq trimmed 0 sp))
         (arg (and sp (string-trim " " (subseq trimmed sp)))))
    (cond
      ((not (and (plusp (length trimmed)) (char= (char trimmed 0) #\/))) nil)
      ((member verb '("/quit" "/exit" "/q") :test #'string-equal) :quit)
      ((string-equal verb "/help") (cmd-help) t)
      ((string-equal verb "/clear")
       (setf (session-history sess) nil)
       (format t "~&~A~%" (paint "— conversation cleared —" :gray)) t)
      ((string-equal verb "/cost") (cmd-cost sess) t)
      ((string-equal verb "/model") (cmd-model arg) t)
      ((string-equal verb "/system") (cmd-system sess) t)
      ((string-equal verb "/tools") (cmd-tools) t)
      (t (format t "~&unknown command ~A — try /help~%" (paint verb :yellow)) t))))

;;; ------------------------------- input ------------------------------

(defvar *linedit* nil "The LINEDIT reader if the library loaded, else NIL.")

(defun try-load-linedit ()
  "Load linedit for line editing/history — but only on an interactive tty.
   Under a pipe or --non-interactive we want the plain read-line path."
  (when (and (not *linedit*) (stdin-tty-p))
    (ignore-errors
     (funcall (read-from-string "ql:quickload") "linedit" :silent t)
     (setf *linedit* (find-symbol "LINEDIT" "LINEDIT")))))

(defun %plain-read (prompt)
  (write-string prompt) (force-output)
  (read-line *standard-input* nil nil))

(defun read-input (prompt)
  "Read one line of input with editing/history if linedit is present, else a
   plain prompt. Returns the line, or NIL on EOF (Ctrl-D) — which the REPL
   treats as quit. Ctrl-D must take exactly ONE press: linedit signals EOF as
   an END-OF-FILE condition (or returns NIL), and either way we propagate that
   as NIL rather than silently retrying with read-line (which would swallow the
   first Ctrl-D and demand a second). A genuine, non-EOF linedit failure falls
   back to read-line once."
  (if *linedit*
      (handler-case
          (funcall *linedit* :prompt prompt :history #P"~/.operandi/history")
        (end-of-file () nil)          ; Ctrl-D -> quit, don't re-read
        (error () (%plain-read prompt)))
      (%plain-read prompt)))

(defun prompt-string (sess)
  (let ((cost (llm:usage-cost-usd (session-usage sess))))
    (format nil "~A~A ~A "
            (paint (model-label) :green)
            (if (plusp cost)
                (paint (format nil " ~,2F¢" (* 100 cost)) :gray) "")
            (paint "›" :br-cyan))))

;;; -------------------------------- repl ------------------------------

(defun banner ()
  (format t "~&~A ~A~%~A~%~%"
          (paint "operandi" :bold :cyan)
          (paint (format nil "· ~A" (model-label)) :gray)
          (paint "Type a task, or /help for commands. Ctrl-C aborts a turn, Ctrl-D quits."
                 :gray)))

(defun repl (&key (greet t))
  "Start the interactive operandi REPL. Reads a task per line, runs the
   agent with live streaming + tool rendering, and keeps conversation
   history across turns until /clear."
  (try-load-linedit)
  (let ((sess (make-session)))
    (when greet (banner))
    (loop
      (let ((line (handler-case (read-input (prompt-string sess))
                    (#+sbcl sb-sys:interactive-interrupt #-sbcl error ()
                      ;; Ctrl-C at an empty prompt: just start a new line.
                      (terpri) ""))))
        (cond
          ((null line) (terpri) (return))            ; EOF / Ctrl-D
          ((zerop (length (string-trim " " line))))  ; blank → reprompt
          (t
           (let ((cmd (handle-command line sess)))
             (cond
               ((eq cmd :quit) (return))
               (cmd)                                  ; command handled
               (t (run-turn sess line))))))))         ; hand to the agent
    (when greet
      (format t "~&~A~%" (paint "bye." :gray)))))
