;;; bin/operandi.lisp
;;;
;;; CLI for standalone operandi.
;;;
;;; Usage (bin/operandi is the launcher; it dumps a core image and runs this
;;; file from it, so the sbcl invocation below is the slow path, not the way
;;; you normally start operandi):
;;;
;;;   bin/operandi [FLAGS] ["task"]
;;;   bin/operandi --help [TOPIC]
;;;
;;;   sbcl --non-interactive --load bin/operandi.lisp -- [FLAGS] ["task"]
;;;
;;; With a prompt (bare, or --prompt/-p) the agent loop runs once and prints
;;; the final answer. With no prompt at all the interactive TUI opens — that
;;; is the default, and --repl only says so. There are no bare command words:
;;; every mode is a flag (--repl, --ask, --resume), so a bare word is always
;;; prompt text.
;;;
;;; --help prints the full list of flags, commands and environment
;;; variables; keep %USAGE below in step with the parser.
;;;
;;; This entry point loads ONLY operandi and its own store, so the Eval
;;; tool reaches operandi.store plus the CL standard library. A host
;;; application that wants Eval to reach richer domain packages writes
;;; its own loader: quickload :operandi, load the domain systems, then
;;; call (operandi.engine:run ...).

(require :asdf)
(unless (find-package :ql)
  (let ((init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file init) (load init))))

;; Load the library — UNLESS it's already present, e.g. we were launched from a
;; dumped core (bin/operandi) that baked it in. Skipping the quickload there is
;; what turns a ~16s cold start into ~20ms. Prefer Quicklisp (resolves deps);
;; fall back to a bare ASDF load-system if the project is registered but ql is
;; absent.
(unless (find-package :operandi.engine)
  (handler-case
      (funcall (read-from-string "ql:quickload") :operandi :silent t)
    (error ()
      (asdf:load-system :operandi))))

;; Open operandi's own store so the tool-call log + Eval have *db* live
;; without manual setup. FIND-SYMBOL so the reader needn't intern the
;; package at read-time.
(funcall (find-symbol "OPEN-STORE" "OPERANDI.STORE"))

;; Under --disable-debugger an unhandled error in ANY thread exits the image;
;; make a stray thread's error kill only that thread (see the function).
(funcall (find-symbol "INSTALL-THREAD-ERROR-GUARD" "OPERANDI.SUBAGENT"))

(defpackage #:operandi-cli
  (:use #:cl)
  (:local-nicknames (#:eng #:operandi.engine)
                    (#:session #:operandi.session)
                    (#:llm #:operandi.llm)
                    (#:json #:json-simple)
                    (#:tools #:operandi.tools)))
(in-package #:operandi-cli)

(defun %csv (s) (and s (mapcar (lambda (x) (string-trim " " x)) (uiop:split-string s :separator ","))))
(defun %stderr-dim-p ()
  "T when stderr is a terminal, so the reasoning trace can be dimmed there
   without leaving escape codes in a redirected log."
  (let ((f (find-symbol "FD-TTY-P" "OPERANDI.TUI")))
    (and f (ignore-errors (funcall f 2 *error-output*)))))

(defun display-run (prompt &optional (tool-names (tools:default-tools)))
  "Run PROMPT streaming tokens live to stdout, then print metrics. The
   final answer is reprinted only if it differs from what already streamed
   (a synthetic '[max-iterations…]' result, or blocking mode).  TOOL-NAMES is the
   available-tools allow-list handed to the agent.

   The model's reasoning trace goes to STDERR, dimmed: it is the scratchpad,
   not the answer, and on a tool-calling turn it is often the ONLY place the
   model says what it is about to do.  Keeping it off stdout means a piped
   `operandi \"task\" > answer.txt` still gets just the answer.

   The run is SAVED as a session, the same record the TUI keeps, so a run that
   hits the iteration cap (or is killed) can be picked up with
   `operandi --resume ID \"write up what you have\"` rather than started over."
  (let* ((saved (session:make-session))
         (buf (make-string-output-stream))
         (dim (%stderr-dim-p))
         (think-open nil)
         (think-line (make-string-output-stream))
         (eng:*on-token* (lambda (s) (write-string s buf) (write-string s) (force-output)))
         ;; durable DURING the run, as the TUI's turns are: a killed run leaves its
         ;; in-flight history on disk, not nothing
         (eng:*on-progress* (lambda (live) (session:checkpoint-session saved live)))
         (eng:*on-reasoning*
           (lambda (s)
             ;; Reasoning arrives in token-sized fragments; buffer to the next
             ;; newline so the trace reads as lines, not one line per token.
             (unless think-open
               (format *error-output* "~&~@[~C[2m~]✻ thinking~@[~C[0m~]~%"
                       (and dim #\Escape) (and dim #\Escape))
               (setf think-open t))
             (loop with start = 0
                   for nl = (position #\Newline s :start start)
                   do (write-string s think-line :start start :end (or nl (length s)))
                      (cond (nl (format *error-output* "~@[~C[2m~]│ ~A~@[~C[0m~]~%"
                                        (and dim #\Escape)
                                        (get-output-stream-string think-line)
                                        (and dim #\Escape))
                                (setf start (1+ nl)))
                            (t (return)))))))
    (multiple-value-bind (text history iters usage) (eng:run prompt :tool-names tool-names)
      ;; commit the run to its session, as RUN-TURN does for a TUI turn
      (setf (session:session-history saved) history)
      (incf (gethash :turns saved))
      (session:session-add-usage saved usage)
      (session:persist-session saved)
      (when think-open
        (let ((tail (get-output-stream-string think-line)))
          (when (plusp (length tail))
            (format *error-output* "~@[~C[2m~]│ ~A~@[~C[0m~]~%" (and dim #\Escape) tail (and dim #\Escape)))))
      ;; BUF holds EVERY turn's streamed text, not just the final one, so the
      ;; answer is already on screen when it is the tail of BUF.  Comparing for
      ;; equality reprinted it after any multi-turn run (the wrap-up made that
      ;; common); a suffix test does not.  Reprint only what did not stream
      ;; (a synthetic or salvaged answer).
      (let* ((streamed (string-right-trim '(#\Space #\Newline) (get-output-stream-string buf)))
             (final (string-right-trim '(#\Space #\Newline) (or text "")))
             (n (length final)))
        (unless (and (<= n (length streamed))
                     (string= final streamed :start2 (- (length streamed) n)))
          (format t "~&~A" text)))
      (format t "~&~%[~A iters, ~A]~%" iters (llm:usage-summary usage))
      (format t "~&session ~A saved; to continue: operandi --resume ~A \"<what to do next>\"~%"
              (gethash :id saved) (gethash :id saved))
      (let ((err (funcall (find-symbol "MIRROR-TURN" "OPERANDI.RC") prompt (or text ""))))
        (when err (format *error-output* "~&operandi: ~A~%" err))))))

(defun run-once (prompt &optional (tool-names (tools:default-tools)))
  (handler-case (display-run prompt tool-names)
    (#+sbcl sb-sys:interactive-interrupt #-sbcl error ()
      (format t "~&interrupted.~%")
      (sb-ext:exit :code 130))))

(defun run-shell (&optional resume)
  "Launch the interactive TUI — the enhanced inline REPL with live
   streaming, tool rendering, multi-turn memory, and slash commands.
   RESUME (a session id or :LATEST) continues a saved session."
  (funcall (find-symbol "REPL" "OPERANDI.TUI") :resume resume))

(defun run-resumed-task (resume prompt)
  "Resume a saved session and run PROMPT as one more turn, non-interactively."
  (handler-case
      (funcall (find-symbol "REPL" "OPERANDI.TUI") :resume resume :once prompt :greet nil)
    (#+sbcl sb-sys:interactive-interrupt #-sbcl error ()
      (format t "~&interrupted.~%")
      (sb-ext:exit :code 130))))

(defun preflight-or-die ()
  "Confirm the configured model/backend can actually serve a request BEFORE
   entering the loop, so a bad model slug / provider-allowlist miss / unreachable
   local server fails loud here instead of blank-per-turn. Skippable with
   OPERANDI_NO_PREFLIGHT=1."
  (unless (uiop:getenv "OPERANDI_NO_PREFLIGHT")
    (multiple-value-bind (ok reason) (eng:preflight-model)
      (unless ok
        (format *error-output* "~&operandi: cannot use model ~A on ~A:~%  ~A~%~%"
                (or llm:*llm-model* "(default)")
                (string-downcase (symbol-name llm:*llm-backend*))
                reason)
        (format *error-output*
                "Fix the model (e.g. --openrouter deepseek/deepseek-v4-flash) or the ~
                 backend, then retry. Set OPERANDI_NO_PREFLIGHT=1 to skip this check.~%")
        (sb-ext:exit :code 2)))))

(defun session-id-like (s)
  "True if S looks like a session id (YYYYMMDD-HHMMSS), so `--resume <id>` can
   tell an id apart from a following command/task."
  (and (stringp s) (= (length s) 15) (char= (char s 8) #\-)
       (every #'digit-char-p (remove #\- s))))

(defun config-path ()
  (merge-pathnames ".operandi/config.json" (user-homedir-pathname)))

(defun config-default-model ()
  "The `model` key of ~/.operandi/config.json, or NIL if absent/unreadable.
   \"llama\" means the local llama.cpp backend."
  (let ((p (config-path)))
    (when (probe-file p)
      (handler-case
          (let ((m (gethash "model" (json:parse (uiop:read-file-string p)))))
            (and (stringp m) (plusp (length m)) m))
        (error (e)
          (format *error-output* "operandi: ignoring ~A: ~A~%" p e)
          nil)))))

(defun %tools-allow (csv)
  "The tool set for --tools: exactly the named tools. A name ending in * takes
   every registered tool with that prefix (mcp__rea__*)."
  (tools:expand-tool-names (%csv csv)))

(defun %tools-deny (csv)
  "The tool set for --no-tools: the defaults minus the named tools (globs too)."
  (let ((deny (tools:expand-tool-names (%csv csv))))
    (remove-if (lambda (n) (member n deny :test #'string-equal)) (tools:default-tools))))

;;; The marker a harness looks for on stdout; keep in sync with mcp/server.js.
(defparameter +json-result-marker+ "@@OPERANDI-JSON@@")

(defun %emit-json-result (ht)
  (format t "~&~A ~A~%" +json-result-marker+ (json:stringify ht))
  (finish-output))

(defun run-ask-json ()
  "Read an ask request as JSON from stdin, run OPERANDI.SUBAGENT:ASK-STRUCTURED,
   print the result as one marker line. Exits 1 on any error."
  (handler-case
      (let* ((req (json:parse (uiop:slurp-stream-string *standard-input*)))
             (get (lambda (k) (let ((v (and (hash-table-p req) (gethash k req))))
                                (if (eq v 'null) nil v))))
             (question (funcall get "question"))
             (hyps (funcall get "hypotheses"))
             (synth (multiple-value-bind (v present)
                        (and (hash-table-p req) (gethash "synthesize" req))
                      (if present (and v (not (eq v 'null))) t))))
        (unless (and (stringp question) (plusp (length question)))
          (error "request needs a non-empty \"question\" string"))
        (when (and hyps (not (and (vectorp hyps) (every #'stringp hyps))))
          (error "\"hypotheses\" must be an array of strings"))
        (%emit-json-result
         (funcall (find-symbol "ASK-STRUCTURED" "OPERANDI.SUBAGENT")
                  question
                  :hypotheses (and hyps (plusp (length hyps)) (coerce hyps 'list))
                  :context (funcall get "context")
                  :synthesize synth
                  :worker-model (funcall get "worker_model"))))
    (error (e)
      (%emit-json-result (let ((h (make-hash-table :test 'equal)))
                           (setf (gethash "error" h) (princ-to-string e))
                           h))
      (sb-ext:exit :code 1 :abort t))))

(defun config-value (key)
  "KEY of ~/.operandi/config.json, or NIL if absent/unreadable."
  (let ((p (config-path)))
    (when (probe-file p)
      (handler-case
          (let ((v (gethash key (json:parse (uiop:read-file-string p)))))
            (and (stringp v) (plusp (length v)) v))
        (error () nil)))))

(defun config-backend ()
  "\"ppq\" or \"openrouter\" (the default) from the config's `backend`."
  (let ((b (config-value "backend")))
    (if (and b (string-equal b "ppq")) "ppq" "openrouter")))

(defun config-integer (key)
  "KEY of the config as a non-negative integer, or NIL."
  (let ((p (config-path)))
    (when (probe-file p)
      (handler-case
          (let ((v (gethash key (json:parse (uiop:read-file-string p)))))
            (and (integerp v) (>= v 0) v))
        (error () nil)))))

;; A caller-issued PPQ key (e.g. one per campaign run, so PPQ itself reports
;; that run's spend): a FILE, mode 0600, never the key in the environment.
;; The account (credit id, balance, top-ups) is still the machine's.
(defparameter *ppq-token-override* (uiop:getenv "OPERANDI_PPQ_TOKEN_FILE"))

(defun auto-topup-plan ()
  "(values cents nip05) when this machine may ask its operator for PPQ credit
   on its own: config backend ppq + auto_topup_cents + an operator NIP-05.
   NIL otherwise -- operandi never invoices anyone unless told it may."
  (let ((cents (config-integer "auto_topup_cents"))
        (nip05 (or (config-value "operator_nip05") (uiop:getenv "OPERANDI_NOSTR_NIP05"))))
    (when (and cents (plusp cents) nip05 (string= (config-backend) "ppq"))
      (values cents nip05))))

(defparameter *topup-lock-dir* (merge-pathnames ".operandi/topup.lock/" (user-homedir-pathname))
  "Held (an atomic mkdir) by the ONE operandi process invoicing the operator.
   The in-process single-flight in the engine covers threads; a campaign runs
   many processes, and without this each would send its own invoice.")

(defun %pid-alive-p (pid)
  (and (integerp pid) (probe-file (format nil "/proc/~D/" pid))))

(defun %try-topup-lock ()
  "Take the cross-process top-up lock, clearing one left by a dead process."
  (flet ((take ()
           (handler-case
               (progn (funcall (find-symbol "MKDIR" "SB-POSIX") (namestring *topup-lock-dir*) #o700)
                      (with-open-file (o (merge-pathnames "pid" *topup-lock-dir*) :direction :output
                                                                                :if-exists :supersede)
                        (format o "~D~%" (funcall (find-symbol "GETPID" "SB-POSIX"))))
                      t)
             (error () nil))))
    (or (take)
        (let ((pid (ignore-errors (parse-integer (uiop:read-file-string (merge-pathnames "pid" *topup-lock-dir*))
                                                 :junk-allowed t))))
          (when (and pid (not (%pid-alive-p pid)))
            (ignore-errors (uiop:delete-directory-tree *topup-lock-dir* :validate t))
            (take))))))

(defun %release-topup-lock ()
  (ignore-errors (uiop:delete-directory-tree *topup-lock-dir* :validate t)))

(defun %funded-p ()
  (let ((b (ignore-errors (operandi.payments:ppq-balance))))
    (and (hash-table-p b) (realp (gethash "balance" b)) (> (gethash "balance" b) 0.05))))

(defun auto-topup ()
  "*PAYMENT-REQUIRED-HOOK*: invoice the operator over Nostr for
   auto_topup_cents and wait for payment, re-issuing an expired invoice (up to
   3). True once paid. ONE process invoices at a time (*TOPUP-LOCK-DIR*); the
   others wait for it and retry. All output to stderr: under ACP/MCP stdout is
   protocol."
  (multiple-value-bind (cents nip05) (auto-topup-plan)
    (when cents
      (let ((*standard-output* *error-output*))
        #+sbcl (ignore-errors (require :sb-posix))
        (loop until (%try-topup-lock)
              do (format t "~&[operandi] another operandi is asking for credit; waiting~%")
                 (loop repeat 720 while (probe-file *topup-lock-dir*) do (sleep 5))
                 ;; whoever held it has paid (or given up): retry the request
                 (return-from auto-topup t))
        (unwind-protect
             (if (%funded-p)
                 t                                  ; paid while we waited for the lock
                 (progn
                   (format t "~&[operandi] PPQ credit exhausted: asking ~A for ~D.~2,'0D USD over Nostr~%"
                           nip05 (floor cents 100) (mod cents 100))
                   (loop repeat 3
                         do (let ((paid (operandi.payments:request-ppq-payment
                                         cents :unit :cents :provider :nostr :nip05 nip05
                                         :memo "operandi is out of PPQ credit" :timeout 3600)))
                              (when paid
                                (format t "~&[operandi] paid -- continuing~%")
                                (return t))))))
          (%release-topup-lock))))))

(defun use-model! (m)
  "MODEL M on the configured backend (llama is always the local server)."
  (cond ((string-equal m "llama") (llm:set-backend-llama! :ask nil))
        ((string= (config-backend) "ppq")
         ;; no account yet, but allowed to ask for credit: make one now, so the
         ;; first inference's 402 can be paid instead of failing for want of a key
         ;; (never under a caller-issued key: creating an account would write
         ;; a different account's key into the caller's file)
         (when (and (not *ppq-token-override*)
                    (not (operandi.payments:ppq-account-p)) (auto-topup-plan))
           (handler-case
               (let ((cid (operandi.payments:ppq-create-account)))
                 (format *error-output* "~&[operandi] created a PPQ account (credit id ~A)~%" cid))
             (error (e) (format *error-output* "~&[operandi] could not create a PPQ account: ~A~%" e))))
         (unless (llm:set-backend-ppq! :model m)
           (format *error-output* "operandi: the PPQ backend is configured but unusable (no ~~/.operandi/ppq.token?) -- try `operandi --topup ppq ...`~%")))
        (t (llm:set-backend-openrouter! :model m)))
  (eng:apply-backend-defaults))

(defun run-topup (args)
  "operandi --topup ppq AMOUNT [cents|sats] [--to NIP05] [--no-wait]

   Buy PPQ credit over Lightning. AMOUNT is an integer in base units: cents
   (the default; 1500 = $15.00) or sats. With no PPQ key on this machine, an
   account is created first (PPQ's /accounts/create needs no credentials). The
   invoice goes to --to, else config `operator_nip05`, else
   $OPERANDI_NOSTR_NIP05, as a Nostr DM -- someone else pays from their phone --
   or, with none of those, is shown here as a QR code. Then waits for payment
   (up to an hour) unless --no-wait, and prints the balance."
  (let* ((to (let ((p (position "--to" args :test #'string=))) (and p (nth (1+ p) args))))
         (no-wait (member "--no-wait" args :test #'string=))
         (pos (remove-if (lambda (a) (or (uiop:string-prefix-p "--" a) (equal a to))) args))
         (service (first pos))
         (amount (and (second pos) (every #'digit-char-p (second pos)) (parse-integer (second pos))))
         (unit (let ((u (string-downcase (or (third pos) "cents"))))
                 (cond ((string= u "cents") :cents) ((string= u "sats") :sats))))
         (nip05 (or to (config-value "operator_nip05") (uiop:getenv "OPERANDI_NOSTR_NIP05"))))
    (unless (and (equal service "ppq") amount (plusp amount) unit)
      (format *error-output* "usage: operandi --topup ppq AMOUNT [cents|sats] [--to NIP05] [--no-wait]   (AMOUNT: an integer; 1500 cents = $15.00)~%")
      (sb-ext:exit :code 2))
    (handler-case
        (progn
          (unless (operandi.payments:ppq-account-p)
            (multiple-value-bind (cid key) (operandi.payments:ppq-create-account)
              (declare (ignore key))
              (format t "~&No PPQ key here: created a PPQ account (credit id ~A); key in ~A~%"
                      cid (namestring operandi.payments:*ppq-token-file*))))
          (let ((paid (operandi.payments:request-ppq-payment
                       amount :unit unit :provider (if nip05 :nostr :terminal) :nip05 nip05
                       :wait (not no-wait) :timeout 3600)))
            (when (and paid (not no-wait))
              (let ((b (ignore-errors (operandi.payments:ppq-balance))))
                (when b (format t "~&Balance: ~A~%" (json:stringify b)))))
            (sb-ext:exit :code (if (or no-wait paid) 0 1))))
      (error (e)
        (format *error-output* "~&operandi --topup: ~A~%" e)
        (sb-ext:exit :code 1)))))

;; May this machine ask its operator for credit? (config auto_topup_cents)
(when (auto-topup-plan)
  (setf eng:*payment-required-hook* #'auto-topup))

;;; ------------------------------ usage ------------------------------
;;; The help text is DATA, not a wall of FORMAT calls.  Each section is
;;; (TOPIC TITLE COLOUR ROWS OVERVIEW-P) and each row is
;;; (LEFT SHORT &rest LONG-LINES): LEFT is the flag or command, SHORT its
;;; one-line description, and LONG-LINES the detail shown only by
;;; `--help TOPIC`.
;;;
;;; The split is the point.  A usage text nobody can scan is a usage text
;;; nobody reads, and a flag that works but is undocumented is a flag nobody
;;; finds — so the top level stays one line per flag (the way ds4's help
;;; reads) and the paragraphs live behind a topic.  Keeping it a table is
;;; also what makes "every flag is documented" checkable by eye instead of by
;;; re-reading the parser.

(defun %paint (str &rest codes)
  "PAINT from operandi.tui, or STR unchanged when the TUI is not loaded.
   Resolved by name so this file does not depend on the TUI's package."
  (let ((f (find-symbol "PAINT" "OPERANDI.TUI")))
    (if (and f (fboundp f))
        (apply (symbol-function f) str codes)
        str)))

(defun %usage-sections ()
  (list
   (list "modes" "Modes" :cyan
         '(("(no prompt)" "interactive TUI — the default")
           ("\"task description\"" "run one task, print the answer, exit"
            "A bare prompt is every remaining argument, joined with spaces. There are"
            "no bare command words: `operandi tui` is a task whose text is \"tui\"."
            "The modes that used to be bare words are flags now — --repl, --ask.")
           ("--prompt TEXT, -p TEXT" "the prompt, spelled out"
            "Same as a bare prompt, for when you would rather not rely on the"
            "leftover-arguments rule. Giving both is an error, not a precedence"
            "puzzle. TEXT is taken as-is, even if it starts with a dash.")
           ("--ask \"question\"" "the question phase: rival hypotheses, a swarm settles them, an answer from the verdicts")
           ("--repl" "the interactive TUI, explicitly"
            "It is already what you get with no prompt; this says so. A prompt"
            "together with --repl is an error.")
           ("--resume [ID]" "resume a saved session (latest if no ID)"
            "With a prompt, continues that session with one more non-interactive"
            "turn; without one, opens it in the TUI."))
         t)

   (list "backends" "Backends" :magenta
         `(("--llama ADDR [-m MODEL]" "the local llama.cpp server (the default)"
            "ADDR is the server's address, not a model: [proto://][host][:port][/path]."
            "A missing scheme means http, a missing host means localhost, a missing"
            "path means /v1/chat/completions, and at least one component is required"
            "— a bare --llama is an error, not a request for the default."
            ""
            "A port must be COLON-PREFIXED: :9931, not 9931. A bare number is"
            "rejected rather than guessed at, because a forgotten colon is far"
            "likelier than a hostname that is all digits."
            ""
            "-m MODEL names the model, for a multi-model server only. Without it the"
            "model is asked of the server: the only one, else the largest, preferring"
            "one it already has loaded — naming an unloaded model makes the router"
            "load a multi-gigabyte file you did not ask for."
            ,(format nil "Default: ~A" llm:*llama-url*))
           ("--openrouter [MODEL]" "OpenRouter; MODEL is a vendor/name slug"
            "Bare --openrouter uses the configured model. Token in"
            "~/.operandi/openrouter.token (mode 600)."
            "Default model: deepseek/deepseek-v4-flash.")
           ("--routstr [NODE-URL] [MODEL]" "Routstr: independent Lightning-paid nodes"
            "NODE-URL (https://...) picks the node; a bare model id uses the default"
            "node. Token in ~/.operandi/routstr.token — a Cashu eCash token or an"
            "sk- key from chat.routstr.com.")
           ("--ppq [MODEL]" "PPQ (ppq.ai), OpenAI-compatible, paid in Bitcoin/Lightning"
            "Any PPQ id (gpt-6.1-sol, private/gpt-oss-120b, ...). Requests go out in"
            "the clear unless --confidential is also given; then only a private/"
            "model is allowed, sealed to Tinfoil's router. Key in ~/.operandi/ppq.token.")
           ("--tinfoil [MODEL]" "Tinfoil's own enclave, sealed end to end"
            "inference.tinfoil.sh. There is no plaintext mode. MODEL is a bare"
            "Tinfoil id (gpt-oss-120b, ...). Key in ~/.operandi/tinfoil.token.")
           ("--maple [ADDR] [MODEL]" "Maple, through a LOCAL maple-proxy"
            "The proxy does Maple's attested enclave transport. operandi speaks plain"
            "HTTP to the proxy and does NOT verify the enclave, so --confidential"
            "refuses it. ADDR is the proxy's address, same grammar as --llama"
            "(default 127.0.0.1:8080). MODEL is a Maple id (gpt-oss-120b, ...)."
            "Optional key in ~/.operandi/maple.token; without it the proxy's own key is used.")
           ("--confidential" "seal every request to an attestation operandi verifies"
            "The endpoint's key must be bound into a hardware attestation checked here."
            "A path that cannot be sealed that way is refused; it FAILS rather than"
            "downgrading to plaintext. The endpoint's operator must also be named in"
            "--trust: the seal proves where the request goes, not what happens there."
            "Today that is Tinfoil's router: --tinfoil, --ppq private/MODEL, or"
            "--routstr tinfoil-MODEL, with --trust=tinfoil-routing,tinfoil-inference.")
           ("--trust=LIST" "the operators you trust with cleartext, comma-separated"
            "tinfoil-inference  Tinfoil's model enclaves and their GPUs."
            "tinfoil-routing    Tinfoil's shared router; it forwards to the model"
            "                   enclaves, so it needs tinfoil-inference."
            "maple-routing      Maple's OpenSecret enclave; it routes some traffic to"
            "                   Tinfoil's router, so it needs tinfoil-routing."
            "A name without what it needs is an error, as is an unknown name. Nothing"
            "reaches tinfoil-inference directly, so it never suffices alone.")))
         t)

   (list "session" "Session" :green
         '(("--rc TARGET" "mirror each turn to a Marmot chat"
            "An npub, nprofile or NIP-05 starts a new chat; a chat name or group id"
            "joins an existing one. Also /rc in the TUI.")
           ("--tui" "an older name for --repl"
            "In the TUI, /help lists the /commands: /model, /effort, /workers,"
            "/sessions, /resume, /cost, /search, /paste, /swarm, /rc, /system,"
            "/tools, /clear, /ask, /quit."))
         t)

   (list "agent" "Agent" :yellow
         '(("--worker-model M" "model for sub-agents: a vendor/name slug, llama, or inherit"
            "inherit (the default) means sub-agents use the orchestrator's model.")
           ("--effort LEVEL" "reasoning effort: off, low, medium, high, default"
            "On OpenRouter, off sends reasoning.enabled=false and the levels send"
            "reasoning.effort. On llama.cpp anything but off/default turns Qwen-style"
            "thinking on. The compaction budget follows the backend (24k tokens on"
            "llama, 96k on OpenRouter) unless OPERANDI_CONTEXT_BUDGET is set.")
           ("--tools A,B,C" "allow-list: exactly these tools")
           ("--no-tools X,Y" "the default tool set minus these")
           ("--mcp-config FILE" "also read FILE's mcpServers (repeatable)"
            "Read after the default ~/.operandi/mcp.json. --no-default-mcp skips the default.")
           ("--mcp NAME=SPEC" "a stdio MCP server, or Streamable HTTP with NAME=https://URL"
            "NAME=COMMAND [ARGS…] runs a stdio server. Its tools join the tool set, so"
            "--tools and --no-tools can name them as mcp__NAME__TOOL. Repeatable.")
           ("--mcp-status" "list the external MCP servers, their state and tools")
           ("--topup ppq AMOUNT [cents|sats]" "buy PPQ credit over Lightning, then exit"
            "AMOUNT is in base units: cents by default (1500 = $15.00), or sats."
            "[--to NIP05] [--no-wait]. The invoice is DMed to --to, else config"
            "operator_nip05, else $OPERANDI_NOSTR_NIP05; with none, shown as a QR code.")
           ("--ask-json" "the question phase as JSON: a request on stdin, one result line"
            "Request: {\"question\": ..., \"hypotheses\": [...]?, \"context\": ...?,"
            "\"synthesize\": bool?, \"worker_model\": ...?}. For harnesses (mcp/server.js).")
           ("--help, -h [TOPIC]" "this text, or one topic of it"
            "Topics: modes, backends, session, agent, env, files, diagnostics.")
         t)

   (list "env" "Environment" :blue
         '(("OPERANDI_MAX_TOKENS" "per-turn output cap (default 16384)")
           ("OPERANDI_CONTEXT_BUDGET" "compaction threshold in tokens")
           ("OPERANDI_CONTEXT_HEADROOM" "fraction of the budget kept free (default 0.2)")
           ("OPERANDI_MAX_ITERS" "hard cap on the agentic loop (default 500)")
           ("OPERANDI_EFFORT" "same as --effort")
           ("OPERANDI_WORKER_MODEL" "same as --worker-model")
           ("OPERANDI_SEARCH" "web-search backend: openrouter, searxng, brave, auto")
           ("OPERANDI_SWARM_DEADLINE" "seconds a Fan/Investigate batch may run (default 900)")
           ("OPERANDI_NO_PREFLIGHT=1" "skip the model preflight")
           ("OPERANDI_FANCY_TUI=1" "opt in to the scroll-region TUI")
           ("OPERANDI_SBCL" "the sbcl binary the launcher uses"))
         nil)

   (list "files" "Files" :white
         '(("~/.operandi/config.json" "default model: {\"model\": \"vendor/name\"} or \"llama\"")
           ("~/.operandi/operandi.db" "saved sessions; --resume continues one")
           ("~/.operandi/offload/" "raw conversation displaced by compaction")
           ("~/.operandi/*.token" "one bearer token per backend, mode 600"))
         nil)

   (list "diagnostics" "Diagnostics" :red
         '(("preflight" "the model is checked before any path that runs a turn"
            "A bad slug, a provider-allowlist miss or an unreachable local server"
            "fails here with exit 2 instead of blank-per-turn. Skip it with"
            "OPERANDI_NO_PREFLIGHT=1.")
           ("unknown --flag" "rejected with exit 2, never sent to the agent as task text")
           ("exit codes" "0 ok, 2 configuration error, 130 interrupted (Ctrl-C)")
           ("entry points" "bin/operandi, bin/operandi-acp, bin/operandi-nostr.lisp"
            "bin/operandi-acp is an ACP server over stdio for editors (Zed, Buzz)."
            "bin/operandi-nostr.lisp is a Nostr DM front end."
            "bin/operandi-sandbox-macos runs this CLI under the macOS sandbox profile."))
         nil)))

(defun %usage-row (left right width)
  (format t "  ~A ~A ~A~%"
          (%paint (format nil "~VA" width left) :br-cyan)
          (%paint "|" :gray)
          right))

(defun %usage-section (section width verbose)
  (destructuring-bind (topic title colour rows overview-p) section
    (declare (ignore topic overview-p))
    (format t "~%~A~%" (%paint title colour :bold))
    (dolist (row rows)
      (%usage-row (first row) (second row) width)
      (when verbose
        (dolist (l (cddr row))
          (if (string= l "")
              (terpri)
              (format t "  ~VA   ~A~%" width "" l)))))))

(defun %usage-more-info (sections)
  (format t "~%~A~%" (%paint "More Info" :bold))
  (dolist (s sections)
    (unless (fifth s)
      (format t "  ~A ~A ~A~%"
              (%paint (format nil "~VA" 12 (concatenate 'string (second s) ":")) :br-cyan)
              (%paint "|" :gray)
              (format nil "--help ~A" (first s)))))
  (format t "  ~A ~A ~A~%"
          (%paint (format nil "~VA" 12 "Everything:") :br-cyan)
          (%paint "|" :gray)
          "--help all"))

(defun %usage-examples ()
  (format t "~%~A~%" (%paint "Examples" :bold))
  (dolist (row '(("chat" "operandi")
                 ("one shot" "operandi \"summarize what's in this directory\"")
                 ("local" "operandi --llama :9931 -m qwen3-32b \"task\"")
                 ("frontier" "operandi --openrouter deepseek/deepseek-v4-flash \"task\"")
                 ("resume" "operandi --resume")
                 ("ask" "operandi --ask \"why does the build fail?\"")
                 ("prompt" "operandi -p \"summarize this directory\"")))
    (format t "  ~A ~A ~A~%"
            (%paint (format nil "~VA" 12 (first row)) :br-cyan)
            (%paint "|" :gray)
            (second row))))

(defun %usage (&optional topic)
  "Print the help text, or one TOPIC of it.  See %USAGE-SECTIONS."
  (let* ((sections (%usage-sections))
         (want (and topic (string-downcase (string topic))))
         (chosen (if (or (null want) (string= want "all"))
                     sections
                     (remove-if-not (lambda (s) (string= (first s) want)) sections)))
         (width (min 30 (max 20 (reduce #'max
                                        (mapcan (lambda (s)
                                                  (mapcar (lambda (r) (length (first r)))
                                                          (fourth s)))
                                                sections))))))
    (unless chosen
      (format *error-output* "~&operandi: no help topic ~S~%  topics: ~{~A~^, ~}~%"
              topic (mapcar #'first sections))
      (sb-ext:exit :code 2))
    (if want
        (dolist (s chosen) (%usage-section s width t))
        (progn
          (format t "~&usage: operandi [FLAGS] [\"task\"]~%")
          (dolist (s sections)
            (when (fifth s) (%usage-section s width nil)))
          (%usage-more-info sections)
          (%usage-examples)))
    (force-output)))

;; Apply the configured default model unless the command line names a backend
;; explicitly (--openrouter, with or without a model; a bare --openrouter also
;; reads the config, below). Doing both would announce the backend twice.
(let ((m (and (not (member-if (lambda (a) (member a '("--llama" "--openrouter" "--routstr" "--ppq" "--tinfoil" "--maple") :test #'string=))
                                 (uiop:command-line-arguments)))
              (config-default-model))))
  (when m
    (if (string-equal m "llama")
        ;; :ASK NIL — the model is resolved once, after parsing, for every path
        ;; that reaches the local server.  Asking here too would be a second
        ;; round-trip and would make the report below claim the model was
        ;; "named with -m" when the server chose it.
        (llm:set-backend-llama! :ask nil)
        (llm:set-backend-openrouter! :model m))
    (eng:apply-backend-defaults)))

(let* ((raw (remove "--" (uiop:command-line-arguments) :test #'string=))
       (resume nil)
       ;; --tui / --repl: force the interactive TUI.  It is also what you get
       ;; when there is no prompt at all, so this only matters when a prompt
       ;; was given too (which is an error — see below).
       (tui-p nil)
       ;; --ask: the question phase instead of a plain task.
       (ask-p nil)
       ;; --prompt/-p TEXT, kept apart from the bare prompt so that giving both
       ;; can be an error rather than a silent choice.
       (prompt-arg nil)
       ;; --help/-h [TOPIC].
       (help-p nil)
       (help-topic nil)
       ;; -m MODEL, remembered so it survives a --llama that comes after it
       ;; (set-backend-llama! otherwise clears the model).
       (llama-model nil)
       ;; Whether --llama was given.  The model is resolved in ONE place after
       ;; parsing (below), for every path that reaches the local server; this
       ;; only decides whether to report the outcome when there is no model to
       ;; report — an explicit --llama deserves an answer, a bare launch does
       ;; not.
       (llama-explicit nil)
       (tool-names (tools:default-tools))     ; available-tools allow-list (default = all)
       ;; --tools / --no-tools are resolved AFTER the MCP servers start: their
       ;; tools join the set, so a name can be an MCP tool (mcp__server__tool).
       (tools-allow nil)
       (tools-deny nil)
       ;; External MCP servers: ~/.operandi/mcp.json unless --no-default-mcp,
       ;; then --mcp-config FILE and --mcp NAME=SPEC, each repeatable, in order.
       (mcp-default t)
       (mcp-configs '())
       (mcp-specs '())
       ;; The modes that used to be bare commands, now flags (a bare word is
       ;; prompt text): --topup ARGS…, --mcp-status, --ask-json.
       (topup-args nil)
       (mcp-status-p nil)
       (ask-json-p nil)
       ;; Strip leading flags (--openrouter [model], --resume [id], --tools/--no-tools) in any order.
       (args (let ((acc raw))
               (loop
                 (cond
                   ((and acc (string= (first acc) "--llama"))
                    ;; The local llama.cpp server — the default backend, so this
                    ;; flag exists to name it explicitly (overriding the config's
                    ;; model) and to point at a server elsewhere.  The argument
                    ;; is the server's ADDRESS, not a model: it is
                    ;; [proto://][host][:port][/path] — ":9931",
                    ;; "localhost:9931", "http://box:9931", or a full endpoint —
                    ;; and at least one component is REQUIRED, so the arity is
                    ;; never ambiguous: a bare --llama is an error, not a
                    ;; request for the default.  A port must be colon-prefixed;
                    ;; a bare number is rejected rather than guessed at.
                    ;;
                    ;; The model is not named here.  A single-model server needs
                    ;; no model field at all, but llama.cpp's ROUTER mode 400s a
                    ;; request that omits one — so when -m did not name a model
                    ;; we ASK the server (GET /v1/models) and take what it
                    ;; serves: the only model, or the largest of several.  -m
                    ;; overrides that.
                    (let* ((tail (rest acc)) (next (first tail)))
                      (unless (and next (not (uiop:string-prefix-p "--" next)))
                        (format *error-output*
                                "operandi: --llama needs an address: [proto://][host][:port][/path] (e.g. --llama :9931)~%")
                        (sb-ext:exit :code 2))
                      (let ((url (handler-case (llm:llama-chat-url next)
                                   (error (e)
                                     (format *error-output* "operandi: ~A~%" e)
                                     (sb-ext:exit :code 2)))))
                        ;; Point at the server and stop there.  The MODEL is
                        ;; resolved once, after parsing, for every path that
                        ;; reaches the local server (see below) — resolving it
                        ;; here too would ask the server twice and could print
                        ;; a model other than the one actually sent.
                        (handler-case
                            (llm:set-backend-llama! :url url :model llama-model :ask nil)
                          (error (e)
                            (format *error-output* "operandi: ~A~%" e)
                            (sb-ext:exit :code 2)))
                        (setf llama-explicit t))
                      (setf acc (rest tail)))
                    (eng:apply-backend-defaults))
                   ;; -m MODEL: the model id to send.  Only meaningful on a
                   ;; multi-model llama.cpp server (router mode, or a proxy in
                   ;; front of several); a single-model server ignores it, and
                   ;; without it the request carries no model field at all.
                   ;; It is remembered in LLAMA-MODEL so it survives a --llama
                   ;; in either order, and it only touches *LLM-MODEL* when the
                   ;; llama backend is the one selected — so `--openrouter X -m Y`
                   ;; does not silently retarget the OpenRouter request.
                   ((and acc (string= (first acc) "-m"))
                    (let ((next (second acc)))
                      (unless (and next (not (uiop:string-prefix-p "--" next)))
                        (format *error-output* "operandi: -m needs a model id~%")
                        (sb-ext:exit :code 2))
                      (setf llama-model next)
                      (when (eq llm:*llm-backend* :llama)
                        (setf llm:*llm-model* next))
                      (setf acc (cddr acc))))
                   ((and acc (string= (first acc) "--openrouter"))
                    (let* ((tail (rest acc)) (next (first tail))
                           ;; OpenRouter model IDs look like 'vendor/model' — a
                           ;; single slashed token. A spaced arg is the task.
                           (is-model (and next (find #\/ next) (not (find #\Space next)))))
                      (cond (is-model (llm:set-backend-openrouter! :model next) (setf acc (rest tail)))
                            ;; bare --openrouter: the configured OpenRouter model, else
                            ;; the library default.
                            (t (let ((cfg (config-default-model)))
                                 (llm:set-backend-openrouter!
                                  :model (if (and cfg (find #\/ cfg)) cfg
                                             "minimax/minimax-m2.7")))
                               (setf acc tail)))
                      (eng:apply-backend-defaults)))
                   ((and acc (string= (first acc) "--routstr"))
                    ;; Routstr, in one of two modes.  A node URL (http...) may
                    ;; come first, then a model.  A model is any token that is
                    ;; not a flag and is followed by the task, or that carries a
                    ;; slash (vendor/model ids); a bare flag takes the library
                    ;; default.  Under --confidential the backend refuses
                    ;; anything that is not a tinfoil-* model, so a typo cannot
                    ;; silently become a plaintext request to a node that
                    ;; terminates it itself.
                    (let* ((tail (rest acc))
                           (node (and (first tail)
                                      (uiop:string-prefix-p "http" (first tail))
                                      (first tail)))
                           (tail (if node (rest tail) tail))
                           (next (first tail))
                           (is-model (and next (not (uiop:string-prefix-p "-" next))
                                          (or (find #\/ next) (rest tail)))))
                      (let ((ok (if is-model
                                    (llm:set-backend-routstr! :model next :url (or node llm:*routstr-url*))
                                    (llm:set-backend-routstr! :url (or node llm:*routstr-url*)))))
                        ;; A refused switch (no key, or a non-tinfoil model
                        ;; under --confidential) must not fall through to
                        ;; whatever backend was configured before — that is
                        ;; exactly the silent downgrade this flag exists to
                        ;; prevent.
                        (unless ok (sb-ext:exit :code 2))
                        (setf acc (if is-model (rest tail) tail)))
                      (eng:apply-backend-defaults)))
                   ((and acc (string= (first acc) "--ppq"))
                    ;; PPQ, in one of two modes. A model here is any PPQ id
                    ;; (gpt-6.1-sol, private/gpt-oss-120b, ...); a bare flag
                    ;; takes the library default. Under --confidential the
                    ;; backend refuses anything that is not private/*, so a
                    ;; typo cannot silently become a plaintext Path B request.
                    ;;
                    ;; Unlike OpenRouter, PPQ ids are often slashless
                    ;; (gpt-6.1-sol), so "has a slash" is not enough to tell a
                    ;; model from the task: a token is taken as the model when
                    ;; it has a slash, or when something follows it (the task).
                    (let* ((tail (rest acc)) (next (first tail))
                           (is-model (and next (not (find #\Space next))
                                          (not (uiop:string-prefix-p "-" next))
                                          (or (find #\/ next) (rest tail)))))
                      (let ((ok (if is-model
                                    (llm:set-backend-ppq! :model next)
                                    (llm:set-backend-ppq!))))
                        ;; A refused switch (no key, or a non-private model
                        ;; under --confidential) must not fall through to
                        ;; whatever backend was configured before — that is
                        ;; exactly the silent downgrade this flag exists to
                        ;; prevent.
                        (unless ok (sb-ext:exit :code 2))
                        (setf acc (if is-model (rest tail) tail)))
                      (eng:apply-backend-defaults)))
                   ((and acc (string= (first acc) "--tinfoil"))
                    ;; Tinfoil's OWN enclave (inference.tinfoil.sh), sealed end
                    ;; to end. There is only one path here, so there is no
                    ;; --confidential to ask for: every request is EHBP-sealed
                    ;; to a locally verified AMD SEV-SNP enclave, and a
                    ;; plaintext answer is an error. A model is a bare Tinfoil
                    ;; id (gpt-oss-120b, glm-5-3, ...); a bare flag takes the
                    ;; library default.
                    (let* ((tail (rest acc)) (next (first tail))
                           ;; Tinfoil ids are bare (gpt-oss-120b), so shape
                           ;; cannot tell a model from the task.  It is a model
                           ;; when it is not a flag and something follows it —
                           ;; `--tinfoil M "task"` vs `--tinfoil "task"`.  A
                           ;; leading --flag is never a model, so a typo still
                           ;; reaches the unknown-option branch below.
                           (is-model (and next (not (uiop:string-prefix-p "-" next))
                                          (rest tail))))
                      (let ((ok (if is-model
                                    (llm:set-backend-tinfoil! :model next)
                                    (llm:set-backend-tinfoil!))))
                        (unless ok (sb-ext:exit :code 2))
                        (setf acc (if is-model (rest tail) tail)))
                      (eng:apply-backend-defaults)))
                   ((and acc (string= (first acc) "--maple"))
                    ;; A local maple-proxy.  ADDR takes --llama's grammar and is
                    ;; recognised by a colon or a scheme with no space in it, so a
                    ;; task that happens to contain a colon is still a task (Maple
                    ;; model ids never contain one).  MODEL is a bare id followed by
                    ;; the task, as for --tinfoil.  A bare --maple is the default proxy.
                    (let* ((tail (rest acc)) (next (first tail))
                           (addr (and next (not (uiop:string-prefix-p "-" next))
                                      (not (find #\Space next))
                                      (or (find #\: next) (uiop:string-prefix-p "http" next))
                                      next))
                           (tail (if addr (rest tail) tail))
                           (next (first tail))
                           (is-model (and next (not (uiop:string-prefix-p "-" next))
                                          (rest tail)))
                           (url (if addr
                                    (handler-case (llm:llama-chat-url addr)
                                      (error (e)
                                        (format *error-output* "operandi: --maple: ~A~%" e)
                                        (sb-ext:exit :code 2)))
                                    llm:*maple-url*)))
                      ;; A refused switch (non-loopback plaintext, or under
                      ;; --confidential) must not fall through to the backend
                      ;; configured before it: that would be a silent downgrade.
                      (let ((ok (if is-model
                                    (llm:set-backend-maple! :url url :model next)
                                    (llm:set-backend-maple! :url url))))
                        (unless ok (sb-ext:exit :code 2))
                        (setf acc (if is-model (rest tail) tail)))
                      (eng:apply-backend-defaults)))
                   ((and acc (string= (first acc) "--confidential-tinfoil"))
                    ;; Replaced, not renamed: the seal and the operator are now two
                    ;; flags.  The old flag errors rather than being accepted.
                    (format *error-output*
                            "~&operandi: --confidential-tinfoil was replaced by --confidential --trust=tinfoil-routing,tinfoil-inference.~%")
                    (sb-ext:exit :code 2))
                   ((and acc (string= (first acc) "--confidential"))
                    ;; Every request sealed to a verified attestation, whose operator is
                    ;; in --trust.  Checked once every flag is parsed (see the startup
                    ;; check below), so the order of the flags does not matter.
                    (setf acc (rest acc))
                    (handler-case (llm:enable-confidential!)
                      (error (e)
                        (format *error-output* "~&operandi: ~A~%" e)
                        (sb-ext:exit :code 2))))
                   ((and acc (or (string= (first acc) "--trust")
                                 (uiop:string-prefix-p "--trust=" (first acc))))
                    ;; --trust=a,b,c or --trust a,b,c.  Unknown names are an error: a typo
                    ;; must not quietly declare nothing.
                    (let* ((inline (uiop:string-prefix-p "--trust=" (first acc)))
                           (spec (if inline (subseq (first acc) (length "--trust=")) (second acc))))
                      (unless (and spec (plusp (length spec)) (not (uiop:string-prefix-p "--" spec)))
                        (format *error-output* "operandi: --trust needs a list of names, e.g. --trust=tinfoil-routing,tinfoil-inference~%")
                        (sb-ext:exit :code 2))
                      (multiple-value-bind (names bad) (llm:parse-trust-list spec)
                        (when bad
                          (format *error-output* "operandi: --trust: unknown name~P ~{~A~^, ~}; known: ~{~A~^, ~}~%"
                                  (length bad) bad (mapcar #'first llm:*trust-parties*))
                          (sb-ext:exit :code 2))
                        (setf llm:*trust* (remove-duplicates (append llm:*trust* names)
                                                             :test #'string= :from-end t)))
                      (setf acc (if inline (rest acc) (cddr acc)))))
                   ((and acc (string= (first acc) "--worker-model"))
                    (multiple-value-bind (m ok)
                        (funcall (find-symbol "PARSE-WORKER-MODEL" "OPERANDI.SUBAGENT") (second acc))
                      (unless ok
                        (format *error-output* "operandi: --worker-model takes a vendor/name slug, llama, or inherit, not ~S~%" (second acc))
                        (sb-ext:exit :code 2))
                      (setf (symbol-value (find-symbol "*WORKER-MODEL*" "OPERANDI.SUBAGENT")) m
                            acc (cddr acc))))
                   ((and acc (string= (first acc) "--effort"))
                    (multiple-value-bind (e ok) (llm:parse-effort (second acc))
                      (unless ok
                        (format *error-output* "operandi: --effort takes off|low|medium|high|default, not ~S~%" (second acc))
                        (sb-ext:exit :code 2))
                      (setf llm:*llm-effort* e acc (cddr acc))))
                   ((and acc (string= (first acc) "--resume"))
                    (let* ((tail (rest acc)) (next (first tail)))
                      (cond ((session-id-like next) (setf resume next    acc (rest tail)))
                            (t                       (setf resume :latest acc tail)))))
                   ;; --tools A,B,C  -> ONLY those tools; --no-tools X,Y -> defaults minus X,Y.
                   ((and acc (string= (first acc) "--tools"))
                    (setf tools-allow (second acc) acc (cddr acc)))
                   ((and acc (string= (first acc) "--no-tools"))
                    (setf tools-deny (second acc) acc (cddr acc)))
                   ;; --model M: M on the CONFIGURED backend (config.json `backend`),
                   ;; so a harness need not know whether this machine pays OpenRouter
                   ;; or PPQ.
                   ((and acc (string= (first acc) "--model"))
                    (unless (and (second acc) (plusp (length (second acc))))
                      (format *error-output* "operandi: --model takes a model id~%")
                      (sb-ext:exit :code 2))
                    (use-model! (second acc))
                    (setf acc (cddr acc)))
                   ;; External MCP servers: --mcp-config FILE (repeatable, after the
                   ;; default ~/.operandi/mcp.json), --mcp NAME=SPEC (repeatable),
                   ;; --no-default-mcp (skip the default file).
                   ((and acc (string= (first acc) "--mcp-config"))
                    (unless (and (second acc) (plusp (length (second acc))))
                      (format *error-output* "operandi: --mcp-config takes a file~%")
                      (sb-ext:exit :code 2))
                    (setf mcp-configs (append mcp-configs (list (second acc))) acc (cddr acc)))
                   ((and acc (string= (first acc) "--mcp"))
                    (unless (and (second acc) (find #\= (second acc)))
                      (format *error-output* "operandi: --mcp takes NAME=COMMAND [ARGS…] or NAME=https://URL~%")
                      (sb-ext:exit :code 2))
                    (setf mcp-specs (append mcp-specs (list (second acc))) acc (cddr acc)))
                   ((and acc (string= (first acc) "--no-default-mcp"))
                    (setf mcp-default nil acc (rest acc)))
                   ;; --topup takes everything after it: operandi --topup ppq 1500 --to NIP05
                   ((and acc (string= (first acc) "--topup"))
                    (setf topup-args (rest acc) acc nil))
                   ((and acc (string= (first acc) "--mcp-status"))
                    (setf mcp-status-p t acc (rest acc)))
                   ((and acc (string= (first acc) "--ask-json"))
                    (setf ask-json-p t acc (rest acc)))
                   ;; --ask: the question phase (rival hypotheses, a swarm
                   ;; settles them, an answer from the verdicts) instead of a
                   ;; plain task.  It used to be the bare command `ask`; a bare
                   ;; word is now always prompt text, so the mode needs a flag.
                   ((and acc (string= (first acc) "--ask"))
                    (setf ask-p t acc (rest acc)))
                   ;; --prompt/-p TEXT: the prompt, spelled out.  A bare prompt
                   ;; still works — every remaining argument is joined into it —
                   ;; so this exists for the cases where you would rather not
                   ;; rely on that, and to make the arity explicit.
                   ;;
                   ;; TEXT is taken as-is, even if it starts with a dash: a
                   ;; value-taking option consumes its argument, so `-p "-x"`
                   ;; is a prompt and not a flag.  Only a MISSING argument is
                   ;; an error.
                   ((and acc (member (first acc) '("--prompt" "-p") :test #'string=))
                    (let ((next (second acc)))
                      (unless next
                        (format *error-output* "operandi: ~A needs a prompt~%" (first acc))
                        (sb-ext:exit :code 2))
                      (setf prompt-arg next acc (cddr acc))))
                   ;; --repl (and its older name --tui): the interactive TUI.
                   ;; It is already what you get with no prompt, so this only
                   ;; says so explicitly — and asking for a TUI AND a prompt is
                   ;; a contradiction, not a choice to make silently (below).
                   ((and acc (member (first acc) '("--repl" "--tui") :test #'string=))
                    (setf tui-p t acc (rest acc)))
                   ;; --help/-h [TOPIC].  Consumed here rather than left in the
                   ;; arguments, so a topic is unambiguous and -h cannot be
                   ;; mistaken for a one-word prompt.
                   ((and acc (member (first acc) '("--help" "-h") :test #'string=))
                    (setf help-p t)
                    (let ((next (second acc)))
                      (when (and next (not (uiop:string-prefix-p "-" next)))
                        (setf help-topic next)))
                    (setf acc (if help-topic (cddr acc) (rest acc))))
                   ;; --rc TARGET: mirror every turn to a Marmot chat (see operandi.rc:rc).
                   ;; npub/nprofile/NIP-05 start a new chat; a hex id or chat name uses an existing one.
                   ((and acc (string= (first acc) "--rc"))
                    (unless (and (second acc) (plusp (length (second acc)))
                                 (not (uiop:string-prefix-p "--" (second acc))))
                      (format *error-output* "operandi: --rc takes an npub, nprofile, NIP-05, chat name or group id~%")
                      (sb-ext:exit :code 2))
                    (handler-case
                        (let ((name (funcall (find-symbol "RC" "OPERANDI.RC") (second acc))))
                          (format t "~&→ rc: mirroring turns to ~A~%" name)
                          ;; Same as /rc in the TUI: the invitee needs this token to drive a turn.
                          (format t "~&  tell them to write ~A in the chat to drive a turn~%"
                                  (symbol-value (find-symbol "*MENTION*" "OPERANDI.RC"))))
                      (error (e)
                        (format *error-output* "operandi: --rc: ~A~%" e)
                        (sb-ext:exit :code 2)))
                    (setf acc (cddr acc)))
                   ;; Any other leading --flag is a typo or unknown option: fail loud
                   ;; instead of sending it to the agent as task text.
                   ((and acc (> (length (first acc)) 2) (string= "--" (first acc) :end2 2))
                    (format *error-output* "operandi: unknown option ~A (try --help)~%" (first acc))
                    (sb-ext:exit :code 2))
                   (t (return))))
               acc))
       ;; The prompt: --prompt/-p if given, else whatever is left over.  There
       ;; are no bare command words any more — `operandi tui` used to mean the
       ;; TUI and `operandi ask "..."` the question phase, but a bare word is
       ;; now always prompt text, so those modes are --repl and --ask.  Giving
       ;; both spellings of the prompt is a mistake, not a precedence puzzle.
       (prompt (cond ((and prompt-arg args)
                      (format *error-output*
                              "operandi: the prompt was given twice — with --prompt and as ~S~%"
                              (format nil "~{~A~^ ~}" args))
                      (sb-ext:exit :code 2))
                     (prompt-arg prompt-arg)
                     (args (format nil "~{~A~^ ~}" args))
                     (t nil))))
  ;; Contradictory modes are a configuration error, and they are checked BEFORE
  ;; the model is resolved: asking the server which model to use, and printing
  ;; it, only to then reject the invocation, is noise on the way to an error.
  ;;
  ;; A TUI and a prompt are contradictory — one is interactive, the other is a
  ;; single turn that exits.  Say so rather than silently dropping one.
  (when (and tui-p prompt)
    (format *error-output*
            "operandi: --repl is interactive, so it takes no prompt (got ~S)~%" prompt)
    (sb-ext:exit :code 2))
  (when (and ask-p (null prompt))
    (format *error-output* "operandi: --ask needs a question~%")
    (sb-ext:exit :code 2))
  ;; --ask runs its own phase against a fresh question; --resume continues a
  ;; saved conversation.  Both at once would silently drop one.
  (when (and ask-p resume)
    (format *error-output* "operandi: --ask and --resume are different modes; pick one~%")
    (sb-ext:exit :code 2))
  ;; Resolve the local server's model in ONE place, for EVERY path that reaches
  ;; it — bare `operandi`, `--llama`, a "llama" config, the TUI.  A single-model
  ;; llama.cpp server needs no model field, but its ROUTER mode 400s a request
  ;; that omits one ("model name is missing from the request"), and the default
  ;; backend IS the local server: without this, a bare launch sent no model at
  ;; all and died at preflight.  -m and a config-resolved model win; we ask the
  ;; server only when nothing named one.
  ;; Not for --help: asking the server is a round-trip, and a usage request
  ;; should not depend on a server being up — nor print a model line above the
  ;; usage text.
  (when (and (eq llm:*llm-backend* :llama) (not help-p))
    (let ((reason (and llm:*llm-model* "named with -m")))
      (when (null llm:*llm-model*)
        (multiple-value-setq (llm:*llm-model* reason)
          (llm:llama-ask-model llm:*llm-url*)))
      ;; An explicit --llama deserves an answer either way; a bare launch only
      ;; speaks up when it found something worth naming.
      (when (or llm:*llm-model* llama-explicit)
        (if llm:*llm-model*
            (format t "~&operandi: llama ~A — model ~A (~A)~%"
                    llm:*llm-url* llm:*llm-model* reason)
            (format t "~&operandi: llama ~A — ~A~%" llm:*llm-url* reason)))))
  ;; Preflight the model on any path that will actually run a turn (not bare help,
  ;; and not topup, which talks to PPQ's billing API and needs no model).
  (unless (or help-p topup-args)
    ;; A confidential session on a backend that cannot seal is a configuration
    ;; error, and it must be fatal BEFORE the first turn — not a warning that
    ;; scrolls past while plaintext goes out.
    (handler-case (llm:require-confidential!)
      (error (e)
        (format *error-output* "~&operandi: ~A~%" e)
        (sb-ext:exit :code 2)))
    (preflight-or-die))
  ;; External MCP servers, before the tool set is resolved: their tools join it.
  ;; A file or spec the operator named that cannot be read is fatal; a server
  ;; that will not start is reported and skipped.
  (unless (or help-p topup-args)
    (handler-case
        (operandi.mcp:start-from-options :use-default mcp-default
                                         :config-files mcp-configs
                                         :cli-specs mcp-specs)
      (error (e)
        (format *error-output* "~&operandi: ~A~%" e)
        (sb-ext:exit :code 2))))
  (setf tool-names (cond (tools-allow (%tools-allow tools-allow))
                         (tools-deny (%tools-deny tools-deny))
                         (t (tools:default-tools))))
  (cond
    (topup-args (run-topup topup-args))
    (mcp-status-p (write-string (operandi.mcp:status-text)) (finish-output))
    ;; operandi --ask-json < request.json: the question phase as DATA, for a
    ;; harness (the MCP server).  One result line on stdout, "@@OPERANDI-JSON@@ {...}".
    (ask-json-p (run-ask-json))
    ;; --help [TOPIC]
    (help-p (%usage help-topic))
    ;; --repl, or no prompt at all → the interactive TUI.  --resume continues a
    ;; saved session there.
    ((or tui-p (null prompt)) (run-shell resume))
    ;; --resume + a prompt → continue that session with one more turn.
    (resume (run-resumed-task resume prompt))
    ;; --ask "why does X happen?" — the question phase, non-interactively.
    (ask-p
     (funcall (find-symbol "REPL" "OPERANDI.TUI")
              :once (format nil "/ask ~A" prompt) :greet nil))
    (t (run-once prompt tool-names))))

;; Exit cleanly once the command is done. Without this, an invocation that
;; lacks --non-interactive (e.g. `sbcl --noinform --load bin/operandi.lisp`)
;; drops into SBCL's own REPL after the TUI quits — forcing an extra Ctrl-D.
;; (`bin/operandi` and the MCP already pass --non-interactive; this is harmless
;; there and makes the bare --load form behave the same.)
;; Tear down any per-run Marmot identity (--rc) before exiting.
(ignore-errors (funcall (find-symbol "DISABLE" "OPERANDI.RC")))
(sb-ext:exit :code 0)
