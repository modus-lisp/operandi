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
                    (#:json  #:json-simple)
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
(deftype user-interrupt ()
  "Ctrl-C, as a condition type.  Only SBCL delivers it as a condition; elsewhere this is the
   EMPTY type, so nothing matches.  The previous fallback was ERROR, which on any other
   implementation caught every genuine failure as though the user had pressed Ctrl-C -- the turn
   was then salvaged and \"[turn interrupted by the user (Ctrl-C)]\" written into the history, a
   false statement the model would go on believing.  An empty type lets a real error propagate
   exactly as it does on SBCL."
  #+sbcl 'sb-sys:interactive-interrupt
  #-sbcl nil)

(defun raw-mode-available-p ()
  "Can the concurrent UI put the terminal into cbreak mode?  It needs termios in-process (see
   RAW-ON for why not `stty'), which here means SBCL's SB-POSIX."
  #+sbcl t #-sbcl nil)

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
(defvar *think-line* nil "Adjustable string: the current line of the model's
  streamed reasoning (the scratchpad), up to the next newline.")
(defvar *think-open* nil "T while a reasoning block is being printed, so the
  header is emitted once and the block is closed before anything else.")

(defparameter *ui-out*
  (lambda (s) (write-string s) (force-output))
  "Sink for ALL agent output — streaming tokens, tool lines, metrics. Default
   is a plain writer (the simple/piped REPL). The concurrent raw-mode UI swaps
   in a locked, scroll-region-aware writer that keeps the pinned input line
   intact. A GLOBAL (not a dynamic binding) because the agent turn runs on a
   worker thread — a let-binding wouldn't reach it.")

(defvar *nl-run* 0 "Trailing newlines in the output so far, capped at 2.")

(defun emit (str)
  "Send STR to the active output sink, updating *AT-BOL* and *NL-RUN* from it."
  (when (and str (plusp (length str)))
    (funcall *ui-out* str)
    (setf *at-bol* (char= (char str (1- (length str))) #\Newline))
    (let ((last-text (position #\Newline str :test-not #'char= :from-end t)))
      (setf *nl-run* (min 2 (if last-text
                                (- (length str) 1 last-text)
                                (+ *nl-run* (length str))))))))

(defun ensure-blank-line ()
  "Leave a blank line before the next block, unless one is already there."
  (fresh)
  (when (< *nl-run* 2) (emit (string #\Newline))))

(defun fresh ()
  "Ensure the cursor is at column 0 (emit a newline only if it isn't)."
  (unless *at-bol* (emit (string #\Newline))))

;;; ------------------------------ markdown ----------------------------
;;; The model answers in markdown; the terminal shows it as styled text.
;;; Rendering is line-at-a-time: tokens accumulate in *MD-LINE* until a
;;; newline, then the finished line is styled and emitted.  Block rules
;;; (headings, lists, quotes, rules, tables, fenced code) look at the whole
;;; line; inline rules (code, bold, italic, links) run per line.  Styling goes
;;; through PAINT, so with colour off the text still reads as clean plain
;;; text (markers dropped, bullets as •).  No dependency: a hand-rolled
;;; scanner covering the subset models actually emit.

(defvar *md-line* nil
  "Adjustable string: the streamed reply's current line, up to the next newline.")
(defvar *md-fence* nil
  "T while inside a ``` fenced code block (lines there are not inline-styled).")

(defun md-new-buf ()
  (make-array 80 :element-type 'character :adjustable t :fill-pointer 0))

(defun md-prefix-p (prefix s)
  (and (>= (length s) (length prefix)) (string= prefix s :end2 (length prefix))))

(defun md-indent (line)
  (subseq line 0 (- (length line) (length (string-left-trim " \t" line)))))

(defun md-hr-p (trim)
  (let ((core (remove #\Space trim)))
    (and (>= (length core) 3)
         (member (char core 0) '(#\- #\* #\_))
         (every (lambda (c) (char= c (char core 0))) core))))

(defun md-table-sep-p (trim)
  (and (find #\- trim)
       (every (lambda (c) (find c "|-: ")) trim)))

(defparameter +md-colours+
  '(("red" . :red) ("green" . :green) ("yellow" . :yellow) ("blue" . :blue)
    ("magenta" . :magenta) ("purple" . :magenta) ("cyan" . :cyan) ("white" . :white)
    ("gray" . :gray) ("grey" . :gray)))

(defun md-colour-span (s i)
  "If S has a colour span :NAME[text] whose colon is at I, return
   (values SGR-keyword open-bracket-index close-bracket-index); else NIL."
  (let* ((end (position-if-not #'alpha-char-p s :start (1+ i)))
         (kw (and end (< (1+ i) end) (char= (char s end) #\[)
                  (cdr (assoc (string-downcase (subseq s (1+ i) end)) +md-colours+
                              :test #'string=))))
         (close (and kw (position #\] s :start end))))
    (when (and kw close (or (= i 0) (not (alphanumericp (char s (1- i))))))
      (values kw end close))))

(defun md-inline (s)
  "Style the inline markdown in S: `code`, **bold**, *italic*/_italic_, [text](url)."
  (let ((n (length s)))
    (with-output-to-string (o)
      (let ((i 0))
        (loop while (< i n) do
          (let ((c (char s i)))
            (cond
              ;; `code`
              ((char= c #\`)
               (let ((j (position #\` s :start (1+ i))))
                 (cond (j (write-string (paint (subseq s (1+ i) j) :cyan) o)
                          (setf i (1+ j)))
                       (t (write-char c o) (incf i)))))
              ;; **bold**
              ((and (char= c #\*) (< (1+ i) n) (char= (char s (1+ i)) #\*))
               (let ((j (search "**" s :start2 (+ i 2))))
                 (cond ((and j (> j (+ i 2)))
                        (write-string (paint (subseq s (+ i 2) j) :bold) o)
                        (setf i (+ j 2)))
                       (t (write-string "**" o) (incf i 2)))))
              ;; *italic* / _italic_ (underscore only at a word start, so snake_case stays)
              ((or (char= c #\*)
                   (and (char= c #\_) (or (= i 0) (not (alphanumericp (char s (1- i)))))))
               (let ((j (and (< (1+ i) n) (position c s :start (+ i 2)))))
                 (cond ((and j (not (char= (char s (1+ i)) #\Space))
                             (not (char= (char s (1- j)) #\Space)))
                        (write-string (paint (subseq s (1+ i) j) :italic) o)
                        (setf i (1+ j)))
                       (t (write-char c o) (incf i)))))
              ;; :red[text] -- colour span (Streamlit-style), so a reply can ask for colour
              ((and (char= c #\:) (md-colour-span s i))
               (multiple-value-bind (kw open close) (md-colour-span s i)
                 (write-string (paint (subseq s (1+ open) close) kw) o)
                 (setf i (1+ close))))
              ;; [text](url)
              ((char= c #\[)
               (let* ((close (position #\] s :start (1+ i)))
                      (paren (and close (< (1+ close) n) (char= (char s (1+ close)) #\()
                                  (1+ close)))
                      (end (and paren (position #\) s :start paren))))
                 (cond (end (write-string (paint (subseq s (1+ i) close) :underline) o)
                            (write-string (paint (format nil " (~A)" (subseq s (1+ paren) end))
                                                 :gray)
                                          o)
                            (setf i (1+ end)))
                       (t (write-char c o) (incf i)))))
              (t (write-char c o) (incf i)))))))))

(defun md-line (line)
  "Style one complete markdown LINE; updates *MD-FENCE*.  Returns the styled text."
  (let* ((line (coerce line '(simple-array character (*))))   ; box chars need CHARACTER, not BASE-CHAR
         (indent (md-indent line))
         (trim (subseq line (length indent)))
         (n (length trim)))
    (cond
      ((md-prefix-p "```" trim)
       (setf *md-fence* (not *md-fence*))
       (paint "────" :gray))
      (*md-fence* (paint line :cyan))
      ((zerop n) "")
      ((md-hr-p trim) (paint (make-string 24 :initial-element #\─) :gray))
      ;; # heading
      ((let ((hashes (position-if-not (lambda (c) (char= c #\#)) trim)))
         (and hashes (<= 1 hashes 6) (char= (char trim hashes) #\Space)))
       (let ((k (position #\Space trim)))
         (paint (md-inline (subseq trim (1+ k))) :bold)))
      ;; > quote
      ((md-prefix-p ">" trim)
       (concatenate 'string (paint "│ " :gray)
                    (paint (md-inline (string-left-trim " " (subseq trim 1))) :italic)))
      ;; - / * / + bullet
      ((and (>= n 2) (find (char trim 0) "-*+") (char= (char trim 1) #\Space))
       (concatenate 'string indent (paint "•" :cyan) " " (md-inline (subseq trim 2))))
      ;; 1. / 1) numbered item
      ((let ((p (position-if-not #'digit-char-p trim)))
         (and p (> p 0) (< (1+ p) n) (find (char trim p) ".)")
              (char= (char trim (1+ p)) #\Space)))
       (let ((p (position-if-not #'digit-char-p trim)))
         (concatenate 'string indent (paint (subseq trim 0 (1+ p)) :cyan) " "
                      (md-inline (subseq trim (+ p 2))))))
      ;; | table | row |
      ((char= (char trim 0) #\|)
       (if (md-table-sep-p trim)
           (paint (substitute #\│ #\| (substitute #\─ #\- trim)) :gray)
           (md-inline (substitute #\│ #\| trim))))
      (t (concatenate 'string indent (md-inline trim))))))

(defun md-feed (tok)
  "Stream TOK through the renderer, emitting each line as its newline arrives."
  (unless *md-line* (setf *md-line* (md-new-buf)))
  (loop with start = 0
        for nl = (position #\Newline tok :start start)
        do (loop for i from start below (or nl (length tok))
                 do (vector-push-extend (char tok i) *md-line*))
           (cond (nl (emit (concatenate 'string (md-line (subseq *md-line* 0))
                                        (string #\Newline)))
                     (setf (fill-pointer *md-line*) 0)
                     (setf start (1+ nl)))
                 (t (return)))))

(defun md-flush ()
  "Emit the partial last line of a segment (no newline yet) and reset the
   per-segment state.  Called before a tool line and at the end of a turn."
  (when (and *md-line* (plusp (fill-pointer *md-line*)))
    (emit (md-line (subseq *md-line* 0))))
  (when *md-line* (setf (fill-pointer *md-line*) 0))
  (setf *md-fence* nil))

(defun md-render-text (text)
  "Style a complete markdown TEXT in one go (for a final answer printed whole)."
  (let ((*md-fence* nil))
    (with-output-to-string (o)
      (loop with start = 0
            for nl = (position #\Newline text :start start)
            do (write-string (md-line (subseq text start (or nl (length text)))) o)
               (cond (nl (write-char #\Newline o) (setf start (1+ nl)))
                     (t (return)))))))

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
  (md-flush)
  (think-close)
  ;; Text streamed before a tool call is narration, not the answer: start
  ;; *STREAMED* over so it only ever holds the text since the last tool call.
  (when *streamed* (get-output-stream-string *streamed*))
  (ensure-blank-line)
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
    (emit (format nil "  ~A ~A  ~A~%~%"
                  mark dur
                  (if err (paint prev :red) (paint prev :gray))))))

;;; ------------------------------ streaming ---------------------------

(defun on-token (tok)
  "OPERANDI.ENGINE:*ON-TOKEN* callback: stream TOK live and capture it."
  (when (and tok (plusp (length tok)))
    (think-close)
    (when *need-bullet*
      (fresh)
      (emit (paint "⏺ " :br-cyan))
      (setf *need-bullet* nil))
    (write-string tok *streamed*)
    (md-feed tok)))

;;; ------------------------------ thinking ----------------------------
;;; A reasoning model's scratchpad (delta.reasoning_content) is where the
;;; commentary around a tool call actually lives: on a tool-calling turn the
;;; model often emits NO content at all, only the trace and the call.  It is
;;; shown dimmed and indented, under a "thinking" header, so it reads as
;;; scratchpad rather than as the answer.

(defun think-open ()
  (unless *think-open*
    (fresh)
    (emit (paint "  ✻ thinking" :gray))
    (emit (string #\Newline))
    (setf *think-open* t)))

(defun think-close ()
  "Close an open reasoning block (no-op when none is open)."
  (when *think-open*
    (when (and *think-line* (plusp (fill-pointer *think-line*)))
      (emit (paint (format nil "  │ ~A" (subseq *think-line* 0)) :gray))
      (emit (string #\Newline))
      (setf (fill-pointer *think-line*) 0))
    (setf *think-open* nil)))

(defun on-reasoning (tok)
  "OPERANDI.ENGINE:*ON-REASONING* callback: print the scratchpad dimmed,
   one line at a time, prefixed so it is visually distinct from the answer."
  (when (and tok (plusp (length tok)))
    (think-open)
    (unless *think-line* (setf *think-line* (md-new-buf)))
    (loop with start = 0
          for nl = (position #\Newline tok :start start)
          do (loop for i from start below (or nl (length tok))
                   do (vector-push-extend (char tok i) *think-line*))
             (cond (nl (emit (paint (format nil "  │ ~A" (subseq *think-line* 0)) :gray))
                       (emit (string #\Newline))
                       (setf (fill-pointer *think-line*) 0)
                       (setf start (1+ nl)))
                   (t (return))))))

;;; ------------------------------ session -----------------------------
;;; Session state + persistence live in operandi.session (shared with the ACP
;;; server). The TUI uses it via the `session:` nickname; raw hash-table access
;;; (gethash :cost/:id/:turns …) is still fine since a session is a hash-table.

(defun user-msg (text) (llm:ht "role" "user" "content" text))

(defun model-label ()
  (case llm:*llm-backend*
    (:openrouter (or llm:*llm-model* "openrouter"))
    (:routstr (or llm:*llm-model* "routstr"))
    (:ppq (or llm:*llm-model* "ppq"))
    (t "llama.cpp")))

(defparameter *cut-off-note*
  "[this turn was cut off — the session ended before it finished. The tool calls above did run and their effects stand; anything after them did not happen.]")

(defun salvage-interrupted (before live &key (note "[turn interrupted by the user (Ctrl-C). The tool calls above did run and their effects stand; anything after them did not happen.]"))
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
                (list (llm:ht "role" "assistant" "content" note))))))

(defun add-resume-note (sess)
  "Queue a note telling the model that this session was closed and reopened.
   It is NOT added to the saved history: RUN-TURN-CONTENT splices it into the
   next request only, and every history write strips it back out."
  (setf (gethash :pending-resume-note sess)
        (llm:ht "role" "user"
                "content" (session:resume-note (gethash :closed-at sess)))))

(defun strip-resume-note (sess msgs)
  "MSGS without the pending resume note object, if it was spliced in this turn."
  (let ((note (gethash :sent-resume-note sess)))
    (if note (remove note msgs :test #'eq) msgs)))

(defun repair-in-flight (sess)
  "A resumed session whose last write was a mid-turn checkpoint: close the
   open turn the way Ctrl-C does (stand-in results for dangling tool calls,
   a note saying where it stopped), count it, and persist it clean."
  (when (session:session-in-flight-p sess)
    (let ((hist (session:session-history sess)))
      (setf (session:session-history sess) (salvage-interrupted nil hist :note *cut-off-note*)
            (gethash :in-flight sess) nil)
      (incf (gethash :turns sess))
      (session:persist-session sess)
      (format t "~&~A~%" (paint "  (that session was cut off mid-turn; the work it had done is kept)" :yellow)))))

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

(defun cmd-search (arg)
  (if (null arg)
      (format t "~&search: ~A~%" (paint (operandi.search:backend-status) :cyan))
      (multiple-value-bind (b ok) (operandi.search:parse-backend arg)
        (if ok
            (progn (setf operandi.search:*backend* b)
                   (format t "~&→ search: ~A~%" (paint (operandi.search:backend-status) :cyan)))
            (format t "~&~A~%" (paint "search takes openrouter, searxng, brave, or auto" :yellow))))))

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

(defun ask-prompt-p (prompt)
  (and (stringp prompt)
       (let ((p (string-left-trim " " prompt)))
         (or (string-equal p "/ask") (uiop:string-prefix-p "/ask " p)))))

(defun ask-question (prompt)
  (string-trim " " (subseq (string-left-trim " " prompt) 4)))

(defun verdict-glyph (v)
  (cond ((equal v "confirmed") (paint "✔ confirmed   " :br-green))
        ((equal v "refuted")   (paint "✗ refuted     " :br-red))
        (t                     (paint "? undetermined" :yellow))))

(defun run-ask (sess question)
  "/ask: the harness, not the model, decides to delegate. Ledger ->
   hypotheses -> swarm -> synthesis; the model is only asked for the parts
   it does well when asked (writing rivals, reading verdicts). The swarm
   runs inside an interrupt handler: /ask arrives as a turn, and Ctrl-C
   here must stop the workers (JOIN-OR-STOP) and return to the prompt, not
   take the process down."
  (when (zerop (length question))
    (fresh)
    (emit (paint (format nil "  /ask needs a question — e.g. /ask why does the first map load stall?~%") :yellow))
    (return-from run-ask nil))
  (let ((synthesis
          (handler-case
              (let* ((project (namestring (uiop:getcwd)))
                     (prior-records (operandi.subagent:project-findings project))
                     (prior (and prior-records (operandi.subagent:findings-brief prior-records))))
                (fresh)
                (emit (paint (format nil "⟐ ask: ~A~%" question) :bold :cyan))
                (emit (paint (format nil "  ledger: ~D prior finding~:P for this project~%"
                                     (length prior-records)) :gray))
                (emit (paint (format nil "  writing hypotheses on ~A…~%" (model-label)) :gray))
                (force-output)
                (let ((hyps (operandi.subagent:elicit-hypotheses question :prior prior)))
                  (cond
                    ((null hyps)
                     ;; never do worse than no /ask at all
                     (emit (paint "  no usable hypotheses from the model — answering directly instead~%" :yellow))
                     question)
                    (t
                     (loop for h in hyps for i from 1
                           do (emit (paint (format nil "    ~D. ~A~%" i (oneline h 110)) :gray)))
                     (emit (paint (format nil "  settling ~D in parallel on ~A…~%"
                                          (length hyps)
                                          (or operandi.subagent:*worker-model* (model-label)))
                                  :gray))
                     (force-output)
                     (multiple-value-bind (text records batch debris timed-out)
                         (operandi.subagent:run-investigate
                          hyps
                          (format nil "Project: ~A~%The question being answered: ~A" project question)
                          (operandi.subagent:ask-worker-tools)
                          1)
                       (declare (ignore text))
                       (dolist (r records)
                         (emit (format nil "    ~A ~A ~A~%"
                                       (verdict-glyph (gethash "verdict" r))
                                       (paint (format nil "~6A" (or (gethash "confidence" r) "")) :gray)
                                       (oneline (gethash "hypothesis" r) 90))))
                       (emit (paint (format nil "  swarm: ~A — recorded to the ledger~%"
                                            (llm:usage-summary batch)) :gray))
                       (when timed-out
                         (emit (paint (format nil "  ⏱ the swarm hit its ~Ds deadline — unfinished workers stopped, answering with what came back~%"
                                              operandi.subagent:*swarm-deadline*)
                                      :yellow)))
                       (when debris
                         (emit (paint (format nil "  ⚠ workers left ~D file~:P in the project (not removed): ~{~A~^, ~}~%"
                                              (length debris) debris)
                                      :yellow)))
                       (force-output)
                       (operandi.subagent:synthesis-prompt question prior records))))))
            (user-interrupt ()
              (fresh)
              (emit (paint "  ⎋ ask interrupted — workers stopped.~%" :yellow))
              nil))))
    (when synthesis
      (run-turn-content sess synthesis))))

(defun run-turn (sess prompt)
  "Run one agent turn on PROMPT, streaming to the terminal, threading
   SESS's history and folding usage. Ctrl-C aborts just this turn.
   @path tokens and /paste'd images become image parts of the message.
   A turn beginning /ask goes through the question phase (RUN-ASK)."
  (when (ask-prompt-p prompt)
    (return-from run-turn (run-ask sess (ask-question prompt))))
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
         ;; a pending resume note rides along on this one request, never in the history
         (note (gethash :pending-resume-note sess))
         (messages (progn
                     (setf (gethash :pending-resume-note sess) nil
                           (gethash :sent-resume-note sess) note)
                     (if hist
                         (append hist (and note (list note)) (list (user-msg prompt)))
                         nil)))
         (*at-bol* t)
         (*need-bullet* t)
         (*md-line* (md-new-buf))
         (*md-fence* nil)
         (*think-line* (md-new-buf))
         (*think-open* nil)
         (*streamed* (make-string-output-stream))
         ;; add our renderers alongside whatever hooks are already installed
         ;; (e.g. the SQLite logger); scoped to this turn.
         (hooks:*pre-tool-hooks*  (cons #'tui-pre-hook hooks:*pre-tool-hooks*))
         (hooks:*post-tool-hooks* (cons #'tui-post-hook hooks:*post-tool-hooks*))
         (eng:*stream* t)
         (eng:*on-token* #'on-token)
         (eng:*on-reasoning* #'on-reasoning)
         ;; durable state DURING the turn: a long tool loop that dies
         ;; otherwise leaves no session file at all
         (eng:*on-progress* (lambda (live)
                              (session:checkpoint-session sess (strip-resume-note sess live))))
         ;; ...and the first checkpoint of a turn (the user message) is never
         ;; throttled away by one from the end of the previous turn
         (session:*last-checkpoint* 0)
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
            (handler-bind ((user-interrupt
                             (lambda (c) (declare (ignore c)) (setf live eng:*live-messages*))))
              (eng:run prompt :history messages :verbose nil))
          (setf (session:session-history sess) (strip-resume-note sess hist*))
          (incf (gethash :turns sess))
          (session:session-add-usage sess usage)
          (session:persist-session sess)          ; crash-safe: write after every turn
          ;; If nothing streamed (non-streaming turn, or a salvaged/synthetic
          ;; answer), or the final text diverged from the stream, print it.
          (md-flush)
          (think-close)
          (let ((streamed (string-right-trim '(#\Space #\Newline)
                                             (get-output-stream-string *streamed*)))
                (final (string-right-trim '(#\Space #\Newline) (or text ""))))
            (when (and (plusp (length final)) (string/= final streamed))
              (fresh)
              (emit (paint "⏺ " :br-cyan))
              (emit (md-render-text final))
              (emit (string #\Newline)))
            (when (and (stringp prompt) (operandi.rc:active-p))
              (let ((err (operandi.rc:mirror-turn prompt (if (plusp (length final)) final streamed))))
                (when err
                  (fresh)
                  (emit (paint (format nil "  ⚠ ~A~%" err) :yellow))))))
          (fresh)
          (emit (paint (format nil "  ~A iter · ~A~%~%" iters (llm:usage-summary usage))
                       :gray))
          (force-output))
      (user-interrupt ()
        ;; Keep what the agent got done before the interrupt (see
        ;; SALVAGE-INTERRUPTED); usage for the partial turn isn't recoverable.
        (let ((kept (strip-resume-note sess (salvage-interrupted messages live))))
          (when (> (length kept) (length messages))
            (setf (session:session-history sess) kept)
            (incf (gethash :turns sess))
            (session:persist-session sess)))
        (md-flush)
        (fresh)
        (emit (paint "  ⎋ interrupted — partial turn kept in context." :yellow))
        (emit (string #\Newline))
        (force-output)))))

;;; ------------------------------ commands ----------------------------

(defun cmd-help ()
  (format t "~&~A~%" (paint "commands:" :bold))
  (dolist (row '(("/help"          "show this help")
                 ("/clear"         "forget the conversation, start fresh")
                 ("/sessions [all]" "list recent sessions you can resume (all for every one)")
                 ("/resume [id]"   "resume a saved session (latest if no id)")
                 ("/cost"          "session cost + token totals")
                 ("/paste"         "attach the clipboard image to your next message (Ctrl-V does this inline)")
                 ("/search [be]"   "show or set the web-search backend: openrouter, searxng, brave, auto")
                 ("/model [id]"    "show or switch model (id like vendor/name → OpenRouter)")
                 ("/effort [lvl]"  "show or set reasoning effort: off, low, medium, high, default")
                 ("/workers [m]"   "show or set the subagent model (vendor/name, llama, or inherit)")
                ("/swarm"         "list the Fan/Investigate workers running right now")
                 ("/rc <target|off>" "mirror each turn to a Marmot chat (npub/nprofile/NIP-05 starts a new chat; a chat name or group id joins an existing one; off: stop)")
                 ("/ask <question>" "rival hypotheses -> a swarm settles them -> an answer from the verdicts")
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

(defparameter *sessions-shown* 10
  "How many of the most recent sessions /sessions lists without `all`.")

(defun cmd-sessions (&optional arg)
  "List saved sessions, oldest-to-newest so the freshest is by the prompt.
   Only the *SESSIONS-SHOWN* most recent unless ARG is \"all\"."
  (let* ((all-rows (session:list-sessions))          ; newest first
         (n (length all-rows))
         (want-all (and arg (member arg '("all" "-a" "a") :test #'string-equal)))
         (rows (reverse (if want-all all-rows (subseq all-rows 0 (min n *sessions-shown*))))))
    (cond
      ((null all-rows) (format t "~&~A~%" (paint "(no saved sessions yet)" :gray)))
      (t
       (when (and (not want-all) (> n *sessions-shown*))
         (format t "~&~A~%" (paint (format nil "… ~D older; /sessions all shows every one" (- n *sessions-shown*)) :gray)))
       (dolist (r rows)
         (destructuring-bind (id turns first &optional in-flight) r
           (format t "  ~A  ~A turn~:P  ~A~@[ ~A~]~%"
                   (paint id :cyan) turns (oneline first 56)
                   (and in-flight (paint "(cut off mid-turn)" :yellow)))))))))

(defun cmd-resume (sess arg)
  (let ((id (session:resume-session! sess (or arg :latest))))
    (if id
        (progn
          (format t "~&~A~%"
                  (paint (format nil "— resumed ~A (~A turn~:P) —" id (session:session-turns sess)) :gray))
          (repair-in-flight sess))
        (format t "~&~A~%"
                (paint "no such session to resume — try /sessions" :yellow)))))

(defun effort-label ()
  (if llm:*llm-effort* (string-downcase (symbol-name llm:*llm-effort*)) "default"))

(defun cmd-model (arg)
  (cond
    ((null arg)
     (format t "~&model: ~A  (backend ~A, effort ~A, context budget ~Dk)~%workers: ~A~%"
             (paint (model-label) :cyan) llm:*llm-backend* (effort-label)
             (round eng:*context-token-budget* 1000)
             (paint (or operandi.subagent:*worker-model* "inherit (same as above)") :cyan)))
    ((find #\/ arg)                       ; vendor/model → OpenRouter
     (llm:set-backend-openrouter! :model arg)
     (eng:apply-backend-defaults)
     (format t "~&→ OpenRouter ~A~%" (paint arg :cyan)))
    ((string-equal arg "llama")
     (llm:set-backend-llama!)
     (eng:apply-backend-defaults)
     (format t "~&→ local llama.cpp~%"))
    (t (format t "~&unknown model spec ~S — give a 'vendor/name' id or 'llama'.~%" arg))))

(defun cmd-workers (arg)
  (if (null arg)
      (format t "~&workers: ~A~%" (paint (or operandi.subagent:*worker-model* "inherit — subagents use the orchestrator's model") :cyan))
      (multiple-value-bind (m ok) (operandi.subagent:parse-worker-model arg)
        (if ok
            (progn (setf operandi.subagent:*worker-model* m)
                   (format t "~&→ workers: ~A~%" (paint (or m "inherit") :cyan)))
            (format t "~&~A~%" (paint "workers takes a vendor/name slug, llama, or inherit" :yellow))))))

(defun cmd-rc (arg)
  "/rc TARGET mirrors each turn to a Marmot chat.  TARGET is an npub, nprofile or NIP-05
to start a new chat with that person, or a chat name / group id for an existing one.
/rc off stops; /rc alone shows status."
  (cond
    ((null arg)
     (if (operandi.rc:active-p)
         (format t "~&rc: mirroring to ~A~%" (paint (operandi.rc:status-chat) :cyan))
         (format t "~&rc: off  (/rc TARGET to mirror, /rc off to stop)~%")))
    ((string-equal arg "off")
     (format t "~&~A~%" (if (operandi.rc:disable) "— rc off —" (paint "rc was not on" :gray))))
    (t
     (handler-case
         (let ((name (operandi.rc:rc arg)))
           (format t "~&→ rc: mirroring turns to ~A~%" (paint name :cyan))
           (when operandi.rc:*mention*
             (format t "~&  tell them to write ~A in the chat to drive a turn~%"
                     (paint operandi.rc:*mention* :cyan))))
       (error (e)
         (format t "~&~A~%" (paint (format nil "rc: ~A" e) :yellow)))))))

(defun cmd-swarm ()
  (format t "~&~A~%" (operandi.subagent:swarm-status-text)))

(defun cmd-effort (arg)
  (if (null arg)
      (format t "~&effort: ~A~A~%" (paint (effort-label) :cyan)
              (case llm:*llm-backend*
                ((:openrouter :routstr) "  (OpenRouter/Routstr: off → reasoning disabled; low/medium/high → reasoning.effort)")
                (:ppq "  (ppq: reasoning is the provider model's own; this backend does not set it)")
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
      ((string-equal verb "/search") (cmd-search arg) t)
      ((string-equal verb "/sessions") (cmd-sessions arg) t)
      ((string-equal verb "/resume") (cmd-resume sess arg) t)
      ((string-equal verb "/model") (cmd-model arg) t)
      ((string-equal verb "/effort") (cmd-effort arg) t)
      ((string-equal verb "/workers") (cmd-workers arg) t)
      ((string-equal verb "/swarm") (cmd-swarm) t)
      ((string-equal verb "/rc") (cmd-rc arg) t)
      ((string-equal verb "/system") (cmd-system sess) t)
      ((string-equal verb "/tools") (cmd-tools) t)
      ;; /ask is a TURN, not a command: returning NIL here lets both REPLs
      ;; route it through run-turn, which has the interrupt handling a swarm needs
      ((string-equal verb "/ask") nil)
      (t (format t "~&unknown command ~A — try /help~%" (paint verb :yellow)) t))))

;;; ------------------------------- input ------------------------------

(defvar *history* nil "The prompt's QUILL:HISTORY, loaded from ~/.operandi/history on first use.")

(defun paste-image-key (session)
  "Ctrl-V: save the clipboard image and insert its @path into the buffer, so
   it rides along with the message like a dragged-in file would. With no
   image on the clipboard, beep -- Cmd-V is the terminal's own text paste."
  (let ((path (clipboard-image)))
    (if path
        (quill:insert (quill:session-buffer session) (format nil "@~A " path))
        (progn (write-char (code-char 7)) (force-output)))))

(defvar *keymap*
  (let ((km (quill:make-keymap quill:*default-keymap*)))
    (quill:bind km :c-v 'paste-image-key)
    km))

(defun %plain-read (prompt)
  (write-string prompt) (force-output)
  (read-line *standard-input* nil nil))

(defun read-input (prompt)
  "Read one line of input -- with editing, multi-line (Ctrl-J), history and
   Ctrl-R on a terminal, a plain read-line otherwise (quill decides). Returns
   the line, or NIL on EOF (Ctrl-D on an empty line, one press), which the REPL
   treats as quit. Ctrl-C returns \"\": a fresh prompt. A genuine editor
   failure falls back to read-line once."
  (unless *history*
    (setf *history* (quill:make-history
                     :file (merge-pathnames ".operandi/history" (user-homedir-pathname)))))
  (handler-case
      (multiple-value-bind (line status)
          (quill:read-line-edited :prompt prompt :history *history*
                                  :keymap *keymap*)
        (case status (:ok line) (:interrupt "") (t nil)))
    (error () (%plain-read prompt))))

(defun prompt-string (sess)
  ;; PLAIN — no ANSI, so it reads the same in a pipe. (quill measures SGR in a
  ;; prompt correctly; linedit didn't.) The concurrent UI uses input-prompt instead.
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
  #-sbcl (error "raw terminal mode needs in-process termios, which this implementation lacks")
  #+sbcl (setf *raw-saved* (sb-posix:tcgetattr 0))
  #+sbcl
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
    #+sbcl (ignore-errors (sb-posix:tcsetattr 0 sb-posix:tcsanow *raw-saved*))
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

(defun rc-inbound (text)
  "Called from the cl-marmot sync thread with a chat message that mentions this run's
mention token (operandi.rc:*mention*, e.g. @op-7k3q9z).
Queues it as the next turn (behind any in-flight turn), like typed input.  When idle,
restores the agent cursor first (as UI-SUBMIT does) so the echo is not written onto the
input row and erased by the next repaint.  The echo goes through EMIT, which takes
*ui-lock*, so it runs outside it."
  (let ((busy *turn-active*))
    (unless busy (bt:with-lock-held (*ui-lock*) (resume-agent)))
    (ui-freshline)
    (emit (format nil "~%~A ~A~%~%" (paint (if busy "⧗ queued (chat) ›" "chat ›") :cyan) text))
    (bt:with-lock-held (*turn-lock*)
      (setf *queue* (append *queue* (list text)))
      (bt:condition-notify *turn-cv*))))

(defun ui-submit (sess)
  (let ((line (string-trim " " *buf*)))
    (cond
      ((zerop (length line)) (ui-clear-input))
      ((char= (char line 0) #\/) (ui-command sess line))
      (t
       (let ((busy *turn-active*))
         (unless busy (bt:with-lock-held (*ui-lock*) (resume-agent)))
         (ui-freshline)
         (emit (format nil "~%~A ~A~%~%"
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

(defun banner-model ()
  "Model and backend for the banner, e.g. \"· deepseek/... · openrouter\"."
  (format nil "· ~A~@[ · ~A~]" (model-label)
          (case llm:*llm-backend* (:openrouter "openrouter")
                                  (:routstr (if llm:*confidential* "routstr confidential" "routstr"))
                                  (:ppq (if llm:*confidential* "ppq confidential" "ppq")))))

(defun ui-banner (sess)
  (emit (format nil "~A ~A~%~A~%~A~%"
                (paint "operandi" :bold :cyan)
                (paint (banner-model) :gray)
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
        ;; /rc inbound: a chat message mentioning @operandi queues a turn, same as typed input.
        (operandi.rc:set-inbound #'rc-inbound)
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
          (paint (banner-model) :gray)
          (paint "Type a task, or /help for commands. Ctrl-C aborts a turn, Ctrl-D quits."
                 :gray)
          (paint (format nil "· session log: ~A~A.md" session:*sessions-dir* (gethash :id sess))
                 :gray)))

(defun resume-command (sess)
  "The shell command that brings SESS back, model flag included — a bare
   --resume would reopen it on the default backend, not the one it ran on."
  (format nil "operandi~@[ ~A ~A~]~@[ --effort ~A~] --resume ~A tui"
          (case llm:*llm-backend*
            (:ppq (if llm:*confidential* "--confidential-tinfoil --ppq" "--ppq"))
            (:tinfoil "--tinfoil")
            (:routstr (if llm:*confidential* "--confidential-tinfoil --routstr" "--routstr"))
            (:openrouter "--openrouter")
            (t nil))
          (and (member llm:*llm-backend* '(:openrouter :routstr :ppq :tinfoil)) llm:*llm-model*)
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
  (when (and greet resume)
    (format t "~&~A~%"
            (paint (if resumed
                       (format nil "resumed session ~A — ~A turn~:P" resumed (session:session-turns sess))
                       "no prior session to resume; starting fresh")
                   (if resumed :br-cyan :yellow))))
  (when greet (banner sess))
  (loop
    (let ((line (handler-case (read-input (prompt-string sess))
                  (user-interrupt ()
                    (terpri) ""))))
      (cond
        ((null line) (terpri) (return))
        ((zerop (length (string-trim " " line))))
        (t
         (let ((cmd (handle-command line sess)))
           (cond
             ((eq cmd :quit) (return))
             (cmd)
             (t
              ;; Enter already ended the typed line (cursor at column 0), so
              ;; record that and let ENSURE-BLANK-LINE add exactly one blank line.
              (setf *at-bol* t *nl-run* 1)
              (ensure-blank-line)
              (run-turn sess line))))))))
  (when greet (farewell sess)))

(defun repl (&key (greet t) resume once)
  "Start the interactive operandi REPL. RESUME (a session id or :LATEST) loads a
   saved session first. ONCE (a prompt) runs exactly that one turn and returns
   (the non-interactive `--resume … \"task\"` path). On an interactive tty this
   uses the concurrent UI (pinned input, background worker, type-while-working);
   piped/non-tty falls back to the simple synchronous loop."
  (let* ((sess (session:make-session))
         (resumed (when resume (session:resume-session! sess resume))))
    (when resumed
      (repair-in-flight sess)
      (add-resume-note sess))
    (cond
      (once (run-turn sess once) nil)
      ;; Default: the robust line REPL (plain writes, no cursor positioning) —
      ;; correct on every terminal. The concurrent UI (pinned input line via a
      ;; scroll region + absolute ESC7/ESC8 positioning) gives type-while-working
      ;; but corrupts output under some terminal/multiplexer combos (e.g. tmux
      ;; scroll-region + DECSC interactions), so it's OPT-IN via OPERANDI_FANCY_TUI=1.
      ;; An active --rc / /rc mirror needs the concurrent UI: only it attaches the
      ;; inbound consumer (set-inbound), so an invitee's @op- message can queue a turn.
      ((and (stdin-tty-p) (stdout-tty-p)
            (or (uiop:getenv "OPERANDI_FANCY_TUI") (operandi.rc:active-p))
            (raw-mode-available-p))
       (handler-case (repl-concurrent sess :greet greet :resume resume :resumed resumed)
         (error (e)
           ;; Say why: falling back silently to repl-simple drops inbound (--rc) chat turns.
           (format *error-output* "~&operandi: interactive UI failed (~A); using the simple REPL, which does NOT take chat (--rc) turns~%" e)
           (ignore-errors (raw-off))
           (setf *ui-out* (lambda (s) (write-string s) (force-output)))
           (repl-simple sess :greet greet :resume resume :resumed resumed))))
      (t (repl-simple sess :greet greet :resume resume :resumed resumed)))))
