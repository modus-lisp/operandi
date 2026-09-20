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
                    (#:tools #:operandi.tools)
                    (#:jzon  #:com.inuoe.jzon)
                    (#:bt    #:bordeaux-threads)
                    (#:session #:operandi.session))
  (:export #:repl #:*color* #:clipboard-image))

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

(defparameter *ui-out*
  (lambda (s) (write-string s) (force-output))
  "Sink for ALL agent output — streaming tokens, tool lines, metrics. Default
   is a plain writer (the simple/piped REPL). The concurrent raw-mode UI swaps
   in a locked, scroll-region-aware writer that keeps the pinned input line
   intact. A GLOBAL (not a dynamic binding) because the agent turn runs on a
   worker thread — a let-binding wouldn't reach it.")

(defun emit (str)
  "Send STR to the active output sink, updating *AT-BOL* from its last char."
  (when (and str (plusp (length str)))
    (funcall *ui-out* str)
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
;;; Session state + persistence live in operandi.session (shared with the ACP
;;; server). The TUI uses it via the `session:` nickname; raw hash-table access
;;; (gethash :cost/:id/:turns …) is still fine since a session is a hash-table.

(defun user-msg (text) (llm:ht "role" "user" "content" text))

(defun model-label ()
  (case llm:*llm-backend*
    (:openrouter (or llm:*llm-model* "openrouter"))
    (t "llama.cpp")))

(defun salvage-interrupted (before live)
  "History to keep after Ctrl-C: LIVE (the engine's in-flight message list)
   if it got past BEFORE, made protocol-valid and marked. The work the agent
   did before the interrupt — reads, edits, decisions — stays in context;
   dropping it left the next turn talking to a model that didn't know it had
   just changed the files on disk. A trailing assistant message with
   tool_calls must be followed by one tool result per call, so any call that
   never ran gets a synthetic result saying so."
  (if (or (null live) (<= (length live) (length before)))
      before
      (let* ((last-asst (find "assistant" live :key (lambda (m) (gethash "role" m))
                                                 :test #'equal :from-end t))
             (tcs (and last-asst (eng:message-tool-calls last-asst)))
             (missing (loop for tc in tcs
                            for id = (gethash "id" tc)
                            unless (find-if (lambda (m) (equal (gethash "tool_call_id" m) id))
                                            live)
                              collect id)))
        ;; Tool results are appended one at a time, so a partially-run batch
        ;; has some results present; only the rest need stand-ins.
        (append live
                (mapcar (lambda (id)
                          (eng:tool-result-msg id "[not run: the user interrupted the turn]"))
                        missing)
                (list (llm:ht "role" "assistant"
                              "content"
                              "[turn interrupted by the user (Ctrl-C). The tool calls above did run and their effects stand; anything after them did not happen.]"))))))

;;; ---- images -----------------------------------------------------------
;;; Two ways in: an @path token in the prompt (drag a file onto the terminal
;;; and it pastes its path), or /paste and Ctrl-V, which pull a PNG off the
;;; clipboard via pngpaste into ~/.operandi/paste/ and hand it over the same
;;; way. The message then goes out as a text part plus image_url parts.

(defvar *pending-images* '()
  "Image paths queued by /paste for the next prompt.")

(defparameter *paste-dir*
  (merge-pathnames ".operandi/paste/" (user-homedir-pathname)))

(defun expand-tilde (path)
  (if (and (> (length path) 1) (char= (char path 0) #\~) (char= (char path 1) #\/))
      (namestring (merge-pathnames (subseq path 2) (user-homedir-pathname)))
      path))

(defun resolve-attachments (prompt)
  "(values text image-paths): every whitespace-delimited @token in PROMPT that
   names an existing image file, plus anything queued by /paste. Tokens that
   don't resolve are left alone — @ is common enough in prose. A resolved
   token is rewritten to [attached image N] in the text — numbered, not
   named: left as @name (or even named in the marker) the model reads it
   as a file to go and find, and starts globbing and decoding it instead
   of looking at the image part right next to it."
  (let ((images '()) (out '()) (n 0))
    (dolist (tok (uiop:split-string prompt :separator '(#\Space)))
      (let* ((bare (and (> (length tok) 1) (char= (char tok 0) #\@)
                        (string-right-trim ",.;:!?)" (subseq tok 1))))
             (path (and bare (expand-tilde bare)))
             (true (and path (llm:image-file-p path) (probe-file path))))
        (if true
            (let ((suffix (subseq tok (1+ (length bare)))))   ; the trimmed punctuation
              (push (namestring true) images)
              (push (format nil "[attached image ~D]~A" (incf n) suffix) out))
            (push tok out))))
    (let ((pending (shiftf *pending-images* '())))
      (values (format nil "~{~A~^ ~}~{~%[attached image ~D]~}"
                      (nreverse out)
                      (loop for p in pending collect (incf n)))
              (append (reverse images) pending)))))

(defun clipboard-image ()
  "Save the clipboard's image to a fresh file under *PASTE-DIR* and return
   its path, or (values nil reason)."
  (ensure-directories-exist *paste-dir*)
  (let ((path (namestring (merge-pathnames
                           (multiple-value-bind (s m h d mo y) (get-decoded-time)
                             (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D.png" y mo d h m s))
                           *paste-dir*))))
    (multiple-value-bind (out err code)
        (ignore-errors (uiop:run-program (list "pngpaste" path)
                                         :output :string :error-output :string
                                         :ignore-error-status t))
      (declare (ignore out))
      (cond ((null code) (values nil "pngpaste not found — brew install pngpaste"))
            ((and (zerop code) (probe-file path)) path)
            (t (values nil (string-trim '(#\Newline #\Space)
                                        (if (plusp (length (or err ""))) err "no image on the clipboard"))))))))

(defun cmd-paste ()
  (multiple-value-bind (path reason) (clipboard-image)
    (if path
        (progn (push path *pending-images*)
               (format t "~&~A ~A ~A~%" (paint "📎" :cyan) (paint path :cyan)
                       (paint "— attached to your next message" :gray)))
        (format t "~&~A~%" (paint reason :yellow)))))

(defun show-attachments (paths)
  (dolist (p paths)
    (let ((size (ignore-errors (with-open-file (f p :element-type '(unsigned-byte 8)) (file-length f)))))
      (emit (paint (format nil "  📎 ~A~@[ (~D KB)~]~%" p (and size (round size 1024))) :gray)))))

(defun run-turn (sess prompt)
  "Run one agent turn on PROMPT, streaming to the terminal, threading
   SESS's history and folding usage. Ctrl-C aborts just this turn.
   @path tokens and /paste'd images become image parts of the message."
  (multiple-value-bind (text images) (resolve-attachments prompt)
    (let ((content (handler-case (llm:user-content text images)
                     (error (e)
                       (fresh)
                       (emit (paint (format nil "  could not attach image: ~A~%" e) :yellow))
                       text))))
      (when (and images (not (stringp content))) (show-attachments images))
      (run-turn-content sess content))))

(defun run-turn-content (sess prompt)
  "RUN-TURN proper, on PROMPT as message content (a string or a parts vector)."
  (let* ((hist (session:session-history sess))
         (messages (if hist (append hist (list (user-msg prompt))) nil))
         (*at-bol* t)
         (*need-bullet* t)
         (*streamed* (make-string-output-stream))
         ;; add our renderers alongside whatever hooks are already installed
         ;; (e.g. the SQLite logger); scoped to this turn.
         (hooks:*pre-tool-hooks*  (cons #'tui-pre-hook hooks:*pre-tool-hooks*))
         (hooks:*post-tool-hooks* (cons #'tui-post-hook hooks:*post-tool-hooks*))
         (eng:*stream* t)
         (eng:*on-token* #'on-token)
         (eng:*on-compact*
           (lambda (before after path)
             (fresh)
             (emit (paint (format nil "  ⟲ compacted context ~Dk → ~Dk tokens~@[ (raw kept in ~A)~]~%"
                                  (round before 1000) (round after 1000) path)
                          :gray))
             (force-output)))
         ;; Snapshot of the engine's in-flight history, taken by a HANDLER-BIND
         ;; while still inside ENG:RUN — *live-messages* is bound there, so by
         ;; the time HANDLER-CASE has unwound to its clause it's gone.
         (live nil))
    (handler-case
        (multiple-value-bind (text hist* iters usage)
            (handler-bind ((#+sbcl sb-sys:interactive-interrupt #-sbcl error
                             (lambda (c) (declare (ignore c)) (setf live eng:*live-messages*))))
              (eng:run prompt :history messages :verbose nil))
          (setf (session:session-history sess) hist*)
          (incf (gethash :turns sess))
          (session:session-add-usage sess usage)
          (session:persist-session sess)          ; crash-safe: write after every turn
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
        ;; Keep what the agent got done before the interrupt (see
        ;; SALVAGE-INTERRUPTED); usage for the partial turn isn't recoverable.
        (let ((kept (salvage-interrupted messages live)))
          (when (> (length kept) (length messages))
            (setf (session:session-history sess) kept)
            (incf (gethash :turns sess))
            (session:persist-session sess)))
        (fresh)
        (emit (paint "  ⎋ interrupted — partial turn kept in context." :yellow))
        (emit (string #\Newline))
        (force-output)))))

;;; ------------------------------ commands ----------------------------

(defun cmd-help ()
  (format t "~&~A~%" (paint "commands:" :bold))
  (dolist (row '(("/help"          "show this help")
                 ("/clear"         "forget the conversation, start fresh")
                 ("/sessions"      "list saved sessions you can resume")
                 ("/resume [id]"   "resume a saved session (latest if no id)")
                 ("/cost"          "session cost + token totals")
                 ("/paste"         "attach the clipboard image to your next message (Ctrl-V does this inline)")
                 ("/model [id]"    "show or switch model (id like vendor/name → OpenRouter)")
                 ("/effort [lvl]"  "show or set reasoning effort: off, low, medium, high, default")
                 ("/system"        "print the active system prompt")
                 ("/tools"         "list the tools the agent can call")
                 ("/quit  /exit"   "leave (Ctrl-D also works)")))
    (format t "  ~A  ~A~%"
            (paint (format nil "~12A" (first row)) :cyan) (second row)))
  (format t "~&Type anything else to hand it to the agent. Ctrl-C aborts a running turn.~%"))

(defun cmd-cost (sess)
  (let ((u (session:session-usage sess)))
    (format t "~&~A over ~A turn~:P — ~A~%"
            (paint "session" :bold) (session:session-turns sess) (llm:usage-summary u))))

(defun cmd-sessions ()
  (let ((rows (session:list-sessions)))
    (if (null rows)
        (format t "~&~A~%" (paint "(no saved sessions yet)" :gray))
        (progn
          (format t "~&~A~%" (paint "saved sessions (newest first):" :bold))
          (dolist (r rows)
            (destructuring-bind (id turns first) r
              (format t "  ~A  ~A turn~:P  ~A~%"
                      (paint id :cyan) turns (oneline first 56))))))))

(defun cmd-resume (sess arg)
  (let ((id (session:resume-session! sess (or arg :latest))))
    (if id
        (format t "~&~A~%"
                (paint (format nil "— resumed ~A (~A turn~:P) —" id (session:session-turns sess)) :gray))
        (format t "~&~A~%"
                (paint "no such session to resume — try /sessions" :yellow)))))

(defun effort-label ()
  (if llm:*llm-effort* (string-downcase (symbol-name llm:*llm-effort*)) "default"))

(defun cmd-model (arg)
  (cond
    ((null arg)
     (format t "~&model: ~A  (backend ~A, effort ~A, context budget ~Dk)~%"
             (paint (model-label) :cyan) llm:*llm-backend* (effort-label)
             (round eng:*context-token-budget* 1000)))
    ((find #\/ arg)                       ; vendor/model → OpenRouter
     (llm:use-openrouter :model arg)
     (eng:apply-backend-defaults)
     (format t "~&→ OpenRouter ~A~%" (paint arg :cyan)))
    ((string-equal arg "llama")
     (llm:use-llama)
     (eng:apply-backend-defaults)
     (format t "~&→ local llama.cpp~%"))
    (t (format t "~&unknown model spec ~S — give a 'vendor/name' id or 'llama'.~%" arg))))

(defun cmd-effort (arg)
  (if (null arg)
      (format t "~&effort: ~A~A~%" (paint (effort-label) :cyan)
              (case llm:*llm-backend*
                (:openrouter "  (OpenRouter: off → reasoning disabled; low/medium/high → reasoning.effort)")
                (t "  (llama.cpp: off/default → no thinking; low/medium/high → thinking on)")))
      (multiple-value-bind (e ok) (llm:parse-effort arg)
        (if ok
            (progn (setf llm:*llm-effort* e)
                   (format t "~&→ effort ~A~%" (paint (effort-label) :cyan)))
            (format t "~&~A~%" (paint "effort takes off, low, medium, high, or default" :yellow))))))

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
       (session:reset-session! sess)
       (format t "~&~A~%" (paint "— conversation cleared (new session) —" :gray)) t)
      ((string-equal verb "/cost") (cmd-cost sess) t)
      ((string-equal verb "/paste") (cmd-paste) t)
      ((string-equal verb "/sessions") (cmd-sessions) t)
      ((string-equal verb "/resume") (cmd-resume sess arg) t)
      ((string-equal verb "/model") (cmd-model arg) t)
      ((string-equal verb "/effort") (cmd-effort arg) t)
      ((string-equal verb "/system") (cmd-system sess) t)
      ((string-equal verb "/tools") (cmd-tools) t)
      (t (format t "~&unknown command ~A — try /help~%" (paint verb :yellow)) t))))

;;; ------------------------------- input ------------------------------

(defvar *linedit* nil "The LINEDIT reader if the library loaded, else NIL.")

(defun try-load-linedit ()
  "Load linedit for line editing/history — but only on an interactive tty.
   Under a pipe or --non-interactive we want the plain read-line path.
   Skip the quickload when linedit is already in the image (bin/operandi
   bakes it into the core): quickload would still make ASDF stat the
   sources under ~/quicklisp, which the sandbox hides, and the resulting
   compile error — swallowed here — left us with no line editing at all."
  (when (and (not *linedit*) (stdin-tty-p))
    (ignore-errors
     (unless (find-package "LINEDIT")
       (funcall (read-from-string "ql:quickload") "linedit" :silent t))
     ;; multi-line editing (src/linedit-ext.lisp) — in the core already when
     ;; launched via bin/operandi; loaded here otherwise
     (unless (find-package "OPERANDI.LINEDIT-EXT")
       (let ((ext (asdf:system-relative-pathname :operandi "src/linedit-ext.lisp")))
         (when (probe-file ext) (load ext))))
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
  ;; PLAIN — no ANSI. This is the linedit/simple-REPL prompt, and linedit both
  ;; mis-counts prompt width and leaks stray codes (e.g. a literal "2m") when the
  ;; prompt contains SGR escapes. The concurrent UI uses input-prompt instead.
  (let ((cost (gethash :cost sess)))
    (format nil "~A~A › "
            (model-label)
            (if (plusp cost) (format nil " ~,2F¢" (* 100 cost)) ""))))

;;; ---------------- concurrent raw-mode UI (interactive tty) ----------------
;;; Pins the input on the bottom terminal row (via a scroll region) while agent
;;; output scrolls above it, and runs the agent turn on a WORKER thread so you
;;; can type while it works. Everything you type is echoed into the log and
;;; queued; it's submitted as the next user turn when the current one (with all
;;; its tool use) finishes. One lock serialises every terminal write. tty-only —
;;; the simple synchronous loop handles pipes/non-tty.

(defvar *ui-lock* (bt:make-lock "operandi-ui"))
(defvar *turn-lock* (bt:make-lock "operandi-turn"))
(defvar *turn-cv* (bt:make-condition-variable :name "operandi-turn"))
(defvar *queue* nil "FIFO of pending user prompts (guarded by *turn-lock*).")
(defvar *worker* nil)
(defvar *turn-active* nil "T while a turn is executing (display + ^C target).")
(defvar *quit* nil)
(defvar *idle-saved* nil "T when the agent cursor was ESC7-saved for idle input.")
(defvar *log-bol* t "T if the log's last written char was a newline.")
(defvar *ui-sess* nil)
(defvar *rows* 24) (defvar *cols* 80)
(defvar *buf* "") (defvar *cur* 0)
(defvar *history* nil) (defvar *hist-idx* nil) (defvar *hist-stash* "")
(defvar *raw-saved* nil)

(defun raw-write (s) (write-string s) (force-output))

(defun %stty (args)
  (ignore-errors
   (uiop:run-program (format nil "stty ~A </dev/tty" args)
                     :output :string :ignore-error-status t)))

(defun raw-on ()
  "Put fd 0 into cbreak: no canonical mode, no echo, no signal chars (^C/^D
   arrive as bytes we decode). Done IN-PROCESS via termios — NOT by shelling to
   `stty`, whose tcsetattr from a subprocess can hit SIGTTOU and hang. Leaves
   output processing (OPOST/ONLCR) and ICRNL intact, so agent output still uses
   plain \\n and Enter arrives as newline."
  (setf *raw-saved* (sb-posix:tcgetattr 0))
  (let ((tio (sb-posix:tcgetattr 0)))
    (setf (sb-posix:termios-lflag tio)
          (logandc2 (sb-posix:termios-lflag tio)
                    (logior sb-posix:icanon sb-posix:echo sb-posix:isig)))
    ;; Clear ICRNL so CR and LF stay distinct bytes: Enter (CR) submits the
    ;; line, Ctrl-J (LF) inserts a newline — multiline paste/editing.
    (setf (sb-posix:termios-iflag tio)
          (logandc2 (sb-posix:termios-iflag tio) sb-posix:icrnl))
    (let ((cc (sb-posix:termios-cc tio)))
      (setf (aref cc sb-posix:vmin) 1 (aref cc sb-posix:vtime) 0))
    (sb-posix:tcsetattr 0 sb-posix:tcsanow tio))
  ;; Ask the terminal for bracketed paste: pasted text arrives wrapped in
  ;; ESC[200~ … ESC[201~ so we can insert it verbatim (newlines and all)
  ;; instead of each line's CR submitting the buffer.
  (raw-write (format nil "~A[?2004h" (string #\Escape))))

(defun raw-off ()
  (when *raw-saved*
    (raw-write (format nil "~A[?2004l" (string #\Escape)))
    (ignore-errors (sb-posix:tcsetattr 0 sb-posix:tcsanow *raw-saved*))
    (setf *raw-saved* nil)))

(defun term-size ()
  "(values rows cols) — via `stty size` (a harmless read); defaults 24x80."
  (multiple-value-bind (r c)
      (ignore-errors
       (let ((p (uiop:split-string (string-trim '(#\Newline #\Space #\Return)
                                                (or (%stty "size") ""))
                                   :separator '(#\Space))))
         (values (parse-integer (first p)) (parse-integer (second p)))))
    (if (and (integerp r) (integerp c)) (values r c) (values 24 80))))

(defun input-prompt ()
  (let ((c (and *ui-sess* (gethash :cost *ui-sess*))))
    (if (and c (plusp c)) (format nil "~,2F¢ › " (* 100 c)) "› ")))

(defun paint-input (&key focus)
  "Draw the input line on the bottom row(s) (windowed, caret kept in view).
   Multiline buffers render bottom-anchored: the caret's row sits on the last
   terminal row, earlier lines above it. FOCUS T leaves the terminal caret in
   the input line (idle typing); FOCUS NIL saves/restores the agent cursor so
   streaming output is undisturbed."
  (let* ((e (string #\Escape))
         (p (input-prompt)) (pw (length p))
         (lines (ui-split-lines *buf*))
         (crow (ui-row-of *buf* *cur*))          ; 0-based line the caret is on
         (ccol (- *cur* (ui-line-start *buf* *cur*)))
         (avail (max 1 (- *cols* pw 1)))
         ;; window the caret's line horizontally
         (start (if (< ccol avail) 0 (1+ (- ccol avail))))
         (view (subseq (nth crow lines) start (min (length (nth crow lines))
                                                   (+ start avail))))
         (caret-col (+ 1 pw (- ccol start)))
         ;; rows to show above the caret row (most recent first)
         (above (loop for i from (1- crow) downto (max 0 (- crow (- *rows* 2)))
                      collect (nth i lines)))
         (top-row (- *rows* (length above)))
         (body (with-output-to-string (s)
                 (loop for line in (reverse above)
                       for r from top-row
                       do (format s "~A[~D;1H~A[2K~A" e r e line))
                 (format s "~A[~D;1H~A[2K~A~A" e *rows* e p view))))
    (raw-write (if focus
                   (format nil "~A~A[~D;~DH" body e *rows* caret-col)
                   (format nil "~A7~A~A8" e body e)))))

(defun ui-write-concurrent (s)
  "The *UI-OUT* sink while the concurrent UI is active: write agent output at
   the agent cursor (inside the scroll region), then repaint the pinned input."
  (bt:with-lock-held (*ui-lock*)
    (raw-write s)
    (when (plusp (length s))
      (setf *log-bol* (char= (char s (1- (length s))) #\Newline)))
    (paint-input :focus nil)))

(defun ui-freshline () (unless *log-bol* (emit (string #\Newline))))

(defun ui-enter ()
  (multiple-value-setq (*rows* *cols*) (term-size))
  (let ((e (string #\Escape)))
    ;; scroll region = rows 1..(rows-1); park the agent cursor at its bottom.
    (raw-write (format nil "~A[1;~Dr~A[~D;1H" e (1- *rows*) e (1- *rows*)))))

(defun ui-exit ()
  (let ((e (string #\Escape)))
    (raw-write (format nil "~A[r~A[~D;1H~%" e e *rows*))))  ; reset region, drop below

(defun go-idle ()
  "Save the agent cursor and move the caret into the input line for typing."
  (raw-write (format nil "~A7" (string #\Escape)))
  (setf *idle-saved* t)
  (paint-input :focus t))

(defun resume-agent ()
  "Restore the agent cursor saved by GO-IDLE before streaming resumes."
  (when *idle-saved*
    (raw-write (format nil "~A8" (string #\Escape)))
    (setf *idle-saved* nil)))

(defun ui-repaint () (bt:with-lock-held (*ui-lock*) (paint-input :focus (not *turn-active*))))

;;; --- worker: drains the queue, one turn at a time ---

(defun worker-loop (sess)
  (loop
    (let ((prompt (bt:with-lock-held (*turn-lock*)
                    (loop until (or *quit* *queue*)
                          do (bt:condition-wait *turn-cv* *turn-lock*))
                    (if *quit* (return) (pop *queue*)))))
      (bt:with-lock-held (*ui-lock*) (resume-agent))
      (setf *turn-active* t)
      (let ((aborted (catch 'abort-turn (run-turn sess prompt) nil)))
        (setf *turn-active* nil)
        (when aborted
          (ui-freshline)
          (emit (paint (format nil "  ⎋ interrupted.~%") :yellow))))
      (when (bt:with-lock-held (*turn-lock*) (null *queue*))
        (bt:with-lock-held (*ui-lock*) (go-idle))))))

(defun ui-quit ()
  (bt:with-lock-held (*turn-lock*) (setf *quit* t) (bt:condition-notify *turn-cv*)))

(defun ui-interrupt ()
  "^C: abort the running turn and drop queued input; if idle, clear the line."
  (cond
    (*turn-active*
     (bt:with-lock-held (*turn-lock*) (setf *queue* nil))
     (when (and *worker* (bt:thread-alive-p *worker*))
       (ignore-errors
        (bt:interrupt-thread *worker* (lambda () (throw 'abort-turn :aborted))))))
    (t (setf *buf* "" *cur* 0 *hist-idx* nil) (ui-repaint))))

;;; --- input editing (main thread) ---

(defun ui-split-lines (s)
  "Split S on LF into a list of line strings (no empty trailing element)."
  (let ((out '()) (start 0))
    (loop for i = (position #\Linefeed s :start start)
          do (push (subseq s start (or i (length s))) out)
          until (null i)
          do (setf start (1+ i)))
    (nreverse out)))

(defun ui-line-start (s pos)
  "Index of the start of the line containing position POS."
  (or (position #\Linefeed s :end pos :from-end t) -1))

(defun ui-row-of (s pos)
  "0-based row index of the line containing POS."
  (count #\Linefeed s :end pos))

(defun ui-clear-input () (setf *buf* "" *cur* 0 *hist-idx* nil) (ui-repaint))
(defun ui-insert-string (s)
  "Insert S (possibly multiline) at the caret; returns T if *buf* changed."
  (when (plusp (length s))
    (setf *buf* (concatenate 'string (subseq *buf* 0 *cur*) s (subseq *buf* *cur*))
          *cur* (+ *cur* (length s))
          *hist-idx* nil)
    (ui-repaint)))
(defun ui-insert (ch) (ui-insert-string (string ch)))
(defun ui-backspace ()
  (when (> *cur* 0)
    (setf *buf* (concatenate 'string (subseq *buf* 0 (1- *cur*)) (subseq *buf* *cur*))
          *cur* (1- *cur*))
    (ui-repaint)))
(defun ui-delete ()
  (when (< *cur* (length *buf*))
    (setf *buf* (concatenate 'string (subseq *buf* 0 *cur*) (subseq *buf* (1+ *cur*))))
    (ui-repaint)))
(defun ui-move (d) (setf *cur* (max 0 (min (length *buf*) (+ *cur* d)))) (ui-repaint))
(defun ui-home () (setf *cur* 0) (ui-repaint))
(defun ui-end () (setf *cur* (length *buf*)) (ui-repaint))
(defun ui-kill () (setf *buf* "" *cur* 0) (ui-repaint))
(defun ui-kill-eol () (setf *buf* (subseq *buf* 0 *cur*)) (ui-repaint))

(defun ui-history (d)
  (let ((n (length *history*)))
    (when (plusp n)
      (cond
        ((and (null *hist-idx*) (minusp d))
         (setf *hist-stash* *buf* *hist-idx* (1- n)
               *buf* (nth *hist-idx* *history*) *cur* (length *buf*)))
        ((null *hist-idx*))                       ; down with no history nav: no-op
        (t (let ((ni (+ *hist-idx* (if (minusp d) -1 1))))
             (cond ((< ni 0) (setf *hist-idx* 0))
                   ((>= ni n) (setf *hist-idx* nil *buf* *hist-stash*))
                   (t (setf *hist-idx* ni *buf* (nth ni *history*))))
             (setf *cur* (length *buf*)))))
      (ui-repaint))))

;;; --- submit + commands ---

(defun ui-submit (sess)
  (let ((line (string-trim " " *buf*)))
    (cond
      ((zerop (length line)) (ui-clear-input))
      ((char= (char line 0) #\/) (ui-command sess line))
      (t
       (let ((busy *turn-active*))
         (unless busy (bt:with-lock-held (*ui-lock*) (resume-agent)))
         (ui-freshline)
         (emit (format nil "~A ~A~%"
                       (paint (if busy "⧗ queued ›" "you ›") (if busy :gray :green))
                       line))
         (setf *history* (append *history* (list line)))
         (bt:with-lock-held (*turn-lock*)
           (setf *queue* (append *queue* (list line)))
           (bt:condition-notify *turn-cv*))
         (ui-clear-input))))))

(defun ui-command (sess line)
  (let* ((sp (position #\Space line)) (verb (subseq line 0 (or sp (length line)))))
    (unless *turn-active* (bt:with-lock-held (*ui-lock*) (resume-agent)))
    (ui-freshline)
    (cond
      ((and *turn-active* (member verb '("/clear" "/resume") :test #'string-equal))
       (emit (paint (format nil "  (finish or ^C the current turn before ~A)~%" verb) :yellow))
       (ui-clear-input))
      (t
       (let* ((out (make-string-output-stream))
              (result (let ((*standard-output* out)) (handle-command line sess))))
         (let ((text (get-output-stream-string out)))
           (when (plusp (length text)) (emit text)))
         (ui-clear-input)
         (when (eq result :quit) (ui-quit)))))))

;;; --- key decoding + the main input loop ---

(defun read-paste ()
  "Read the body of a bracketed paste (already past ESC[200~) up to the
   ESC[201~ terminator. Returns the pasted text with CR/CRLF normalised to
   LF; on EOF returns what was gathered."
      (declare (ignore e))
  (let ((acc (make-string-output-stream))
        (e (string #\Escape)))
    (flet ((term-p ()
             ;; peek for the 201~ terminator: ESC [ 2 0 1 ~
             (and (char= (or (read-char *standard-input* nil :eof) :eof) #\Escape)
                  (char= (or (read-char *standard-input* nil :eof) :eof) #\[)
                  (char= (or (read-char *standard-input* nil :eof) :eof) #\2)
                  (char= (or (read-char *standard-input* nil :eof) :eof) #\0)
                  (char= (or (read-char *standard-input* nil :eof) :eof) #\1)
                  (char= (or (read-char *standard-input* nil :eof) :eof) #\~))))
      (loop
        (let ((c (read-char *standard-input* nil :eof)))
          (cond ((eq c :eof) (return))
                ;; CR or CRLF inside a paste becomes a single LF
                ((char= c #\Return)
                 (when (char= (peek-char nil *standard-input* nil :eof) #\Linefeed)
                   (read-char *standard-input*))
                 (write-char #\Linefeed acc))
                (t (write-char c acc)))))
      (get-output-stream-string acc))))

(defun read-csi ()
  (let ((acc (make-string-output-stream)))
    (loop for c = (read-char *standard-input* nil :eof)
          do (cond
               ((eq c :eof) (return :escape))
               ((and (char= c #\2) (char= (peek-char nil *standard-input* nil :eof) #\0))
                ;; 200~ = bracketed paste start
                (read-char *standard-input*)
                (when (char= (read-char *standard-input* nil :eof) #\0)
                  (when (char= (read-char *standard-input* nil :eof) #\~)
                    (return (cons :paste (read-paste)))))
                :ignore)
               ((or (char<= #\A c #\Z) (char<= #\a c #\z) (char= c #\~))
                (return (let ((params (get-output-stream-string acc)))
                          (cond ((char= c #\A) :up) ((char= c #\B) :down)
                                ((char= c #\C) :right) ((char= c #\D) :left)
                                ((char= c #\H) :home) ((char= c #\F) :end)
                                ((and (char= c #\~) (string= params "3")) :delete)
                                ((and (char= c #\~) (member params '("1" "7") :test #'string=)) :home)
                                ((and (char= c #\~) (member params '("4" "8") :test #'string=)) :end)
                                (t :ignore)))))
               (t (write-char c acc))))))

(defun read-key ()
  (let ((c (read-char *standard-input* nil :eof)))
    (cond
      ((eq c :eof) :eof)
      ((char= c #\Escape)
       (let ((n (read-char *standard-input* nil :eof)))
         (if (member n '(#\[ #\O)) (read-csi) :escape)))
      ((or (char= c #\Return) (char= c #\Newline)) :enter)
      ((char= c #\Linefeed) :newline)
      ((or (char= c #\Rubout) (char= c (code-char 8))) :backspace)
      ((char= c (code-char 3)) :ctrl-c)
      ((char= c (code-char 4)) :ctrl-d)
      ((char= c (code-char 1)) :home)
      ((char= c (code-char 5)) :end)
      ((char= c (code-char 21)) :kill)
      ((char= c (code-char 11)) :kill-eol)
      ((char= c (code-char 12)) :redraw)
      ((< (char-code c) 32) :ignore)
      (t c))))

(defun ui-banner (sess)
  (emit (format nil "~A ~A~%~A~%~A~%"
                (paint "operandi" :bold :cyan)
                (paint (format nil "· ~A" (model-label)) :gray)
                (paint "Type while it works — your line queues as the next message. ^C aborts a turn, ^D quits."
                       :gray)
                (paint (format nil "· session log: ~A~A.md" session:*sessions-dir* (gethash :id sess))
                       :gray))))

(defun repl-concurrent (sess &key (greet t) resume resumed)
  "Interactive REPL with a pinned input line + background worker (tty only)."
  (setf *ui-sess* sess *buf* "" *cur* 0 *queue* nil *worker* nil
        *turn-active* nil *quit* nil *idle-saved* nil *log-bol* t
        *history* nil *hist-idx* nil
        *ui-out* #'ui-write-concurrent)
  (raw-on)
  (unwind-protect
      (progn
        (ui-enter)
        (when greet (ui-banner sess))
        (when resume
          (ui-freshline)
          (emit (paint (if resumed
                           (format nil "resumed session ~A — ~A turn~:P~%" resumed (session:session-turns sess))
                           (format nil "no prior session to resume; starting fresh~%"))
                       (if resumed :br-cyan :yellow))))
        (setf *worker* (bt:make-thread (lambda () (worker-loop sess)) :name "operandi-turn"))
        (bt:with-lock-held (*ui-lock*) (go-idle))
        (loop
          (let ((key (read-key)))
            (case key
              (:eof (return))
              (:ctrl-d (when (zerop (length *buf*)) (return)))
              (:ctrl-c (ui-interrupt))
              (:enter (ui-submit sess))
              (:newline (ui-insert-string (string #\Linefeed)))
              (:backspace (ui-backspace))
              (:delete (ui-delete))
              (:left (ui-move -1))
              (:right (ui-move 1))
              (:home (ui-home))
              (:end (ui-end))
              (:kill (ui-kill))
              (:kill-eol (ui-kill-eol))
              (:up (if (plusp (ui-row-of *buf* *cur*))
                       (ui-move (-
                                 (ui-line-start *buf* *cur*)
                                 *cur*))
                       (ui-history -1)))
              (:down (let ((nl (position #\Linefeed *buf* :start *cur*)))
                       (if nl (ui-move (1+ (- nl *cur*))) (ui-history 1))))
              (:redraw (ui-repaint))
              ((:ignore :escape) nil)
              ((cons :paste s) (ui-insert-string s))
              (t (when (characterp key) (ui-insert key))))
            (when *quit* (return)))))
    ;; teardown: stop the worker (aborting a turn in flight), restore terminal.
    (bt:with-lock-held (*turn-lock*) (setf *quit* t *queue* nil) (bt:condition-notify *turn-cv*))
    (when (and *worker* (bt:thread-alive-p *worker*) *turn-active*)
      (ignore-errors (bt:interrupt-thread *worker* (lambda () (throw 'abort-turn :quit)))))
    (ignore-errors (bt:join-thread *worker*))
    (ui-exit)
    (raw-off)
    (setf *ui-out* (lambda (s) (write-string s) (force-output)))
    (when greet (farewell sess))))

;;; -------------------------------- repl ------------------------------

(defun banner (sess)
  (format t "~&~A ~A~%~A~%~A~%~%"
          (paint "operandi" :bold :cyan)
          (paint (format nil "· ~A" (model-label)) :gray)
          (paint "Type a task, or /help for commands. Ctrl-C aborts a turn, Ctrl-D quits."
                 :gray)
          (paint (format nil "· session log: ~A~A.md" session:*sessions-dir* (gethash :id sess))
                 :gray)))

(defun resume-command (sess)
  "The shell command that brings SESS back, model flag included — a bare
   --resume would reopen it on the default backend, not the one it ran on."
  (format nil "operandi~@[ --openrouter ~A~]~@[ --effort ~A~] --resume ~A tui"
          (and (eq llm:*llm-backend* :openrouter) llm:*llm-model*)
          (and llm:*llm-effort* (effort-label))
          (gethash :id sess)))

(defun farewell (sess)
  "Printed on exit. A session with turns is saved and resumable; say how,
   so the id doesn't have to be dug out of /sessions later."
  (if (plusp (or (session:session-turns sess) 0))
      (format t "~&~A~%~A~%  ~A~%~A~%"
              (paint "bye." :gray)
              (paint (format nil "session ~A saved (~A turn~:P). to pick it up again:"
                             (gethash :id sess) (session:session-turns sess))
                     :gray)
              (paint (resume-command sess) :cyan)
              (paint "(or --resume with no id for the latest session; /sessions lists them)" :gray))
      (format t "~&~A~%~A~%"
              (paint "bye." :gray)
              (paint "(no turns, so nothing was saved; `operandi --resume tui` reopens the latest saved session)" :gray))))

(defun repl-simple (sess &key (greet t) resume resumed)
  "The synchronous line-at-a-time REPL: read a task, run it to completion, read
   the next. Used for pipes / non-tty (and as the fallback if raw mode fails).
   No mid-turn typing — the turn blocks the loop."
  (try-load-linedit)
  (when (and greet resume)
    (format t "~&~A~%"
            (paint (if resumed
                       (format nil "resumed session ~A — ~A turn~:P" resumed (session:session-turns sess))
                       "no prior session to resume; starting fresh")
                   (if resumed :br-cyan :yellow))))
  (when greet (banner sess))
  (loop
    (let ((line (handler-case (read-input (prompt-string sess))
                  (#+sbcl sb-sys:interactive-interrupt #-sbcl error ()
                    (terpri) ""))))
      (cond
        ((null line) (terpri) (return))
        ((zerop (length (string-trim " " line))))
        (t
         (let ((cmd (handle-command line sess)))
           (cond
             ((eq cmd :quit) (return))
             (cmd)
             (t (run-turn sess line))))))))
  (when greet (farewell sess)))

(defun repl (&key (greet t) resume once)
  "Start the interactive operandi REPL. RESUME (a session id or :LATEST) loads a
   saved session first. ONCE (a prompt) runs exactly that one turn and returns
   (the non-interactive `--resume … \"task\"` path). On an interactive tty this
   uses the concurrent UI (pinned input, background worker, type-while-working);
   piped/non-tty falls back to the simple synchronous loop."
  (let* ((sess (session:make-session))
         (resumed (when resume (session:resume-session! sess resume))))
    (cond
      (once (run-turn sess once) nil)
      ;; Default: the robust line REPL (plain writes, no cursor positioning) —
      ;; correct on every terminal. The concurrent UI (pinned input line via a
      ;; scroll region + absolute ESC7/ESC8 positioning) gives type-while-working
      ;; but corrupts output under some terminal/multiplexer combos (e.g. tmux
      ;; scroll-region + DECSC interactions), so it's OPT-IN via OPERANDI_FANCY_TUI=1.
      ((and (stdin-tty-p) (stdout-tty-p) (uiop:getenv "OPERANDI_FANCY_TUI"))
       (handler-case (repl-concurrent sess :greet greet :resume resume :resumed resumed)
         (error ()
           (ignore-errors (raw-off))
           (setf *ui-out* (lambda (s) (write-string s) (force-output)))
           (repl-simple sess :greet greet :resume resume :resumed resumed))))
      (t (repl-simple sess :greet greet :resume resume :resumed resumed)))))
