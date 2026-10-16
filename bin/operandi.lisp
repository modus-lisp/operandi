;;; bin/operandi.lisp
;;;
;;; CLI for standalone operandi.
;;;
;;; Usage:
;;;   sbcl --non-interactive --load bin/operandi.lisp -- "your task here"
;;;   sbcl --non-interactive --load bin/operandi.lisp -- shell
;;;   sbcl --non-interactive --load bin/operandi.lisp -- --openrouter [MODEL] "task"
;;;
;;; The single-task form runs the agent loop once on the given prompt
;;; and prints the final answer. The shell form is an interactive REPL:
;;; each input line becomes a fresh agent run; type 'exit' to quit.
;;; Conversation history is NOT carried across shell prompts — each task
;;; is isolated.
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
                    (#:llm #:operandi.llm)
                    (#:json #:json-simple)
                    (#:tools #:operandi.tools)))
(in-package #:operandi-cli)

(defun %csv (s) (and s (mapcar (lambda (x) (string-trim " " x)) (uiop:split-string s :separator ","))))
(defun %tools-allow (csv)
  "The tool set for --tools: exactly the named tools. A name ending in * takes
   every registered tool with that prefix (mcp__rea__*)."
  (tools:expand-tool-names (%csv csv)))
(defun %tools-deny (csv)
  "The tool set for --no-tools: the defaults minus the named tools (globs too)."
  (let ((deny (tools:expand-tool-names (%csv csv))))
    (remove-if (lambda (n) (member n deny :test #'string-equal)) (tools:default-tools))))

(defun display-run (prompt &optional (tool-names (tools:default-tools)))
  "Run PROMPT streaming tokens live to stdout, then print metrics. The
   final answer is reprinted only if it differs from what already streamed
   (a synthetic '[max-iterations…]' result, or blocking mode).  TOOL-NAMES is the
   available-tools allow-list handed to the agent."
  (let* ((buf (make-string-output-stream))
         (eng:*on-token* (lambda (s) (write-string s buf) (write-string s) (force-output))))
    (multiple-value-bind (text history iters usage) (eng:run prompt :tool-names tool-names)
      (declare (ignore history))
      (let ((streamed (string-right-trim '(#\Space #\Newline) (get-output-stream-string buf))))
        (unless (string= (string-right-trim '(#\Space #\Newline) (or text "")) streamed)
          (format t "~&~A" text)))
      (format t "~&~%[~A iters, ~A]~%" iters (llm:usage-summary usage))
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

(defun preflight-or-die ()
  "Confirm the configured model/backend can actually serve a request BEFORE
   entering the loop, so a bad model slug / provider-allowlist miss / unreachable
   local server fails loud here instead of blank-per-turn. Skippable with
   OPERANDI_NO_PREFLIGHT=1."
  (unless (uiop:getenv "OPERANDI_NO_PREFLIGHT")
    (multiple-value-bind (ok reason) (eng:preflight-model)
      ;; out of credit, and allowed to ask for more: ask, then check again
      (when (and (not ok) (eng:payment-required-p reason) (eng:handle-payment-required))
        (multiple-value-setq (ok reason) (eng:preflight-model)))
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
(when (and *ppq-token-override* (plusp (length *ppq-token-override*)))
  ;; both: operandi.ppq's is the one the LLM backend authenticates with,
  ;; operandi.payments' the one top-ups and account creation use
  (setf operandi.ppq:*ppq-token-file* (pathname *ppq-token-override*)
        operandi.payments:*ppq-token-file* (pathname *ppq-token-override*)))

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
  (cond ((string-equal m "llama") (llm:set-backend-llama!))
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
           (format *error-output* "operandi: the PPQ backend is configured but unusable (no ~~/.operandi/ppq.token?) -- try `operandi topup ppq ...`~%")))
        (t (llm:set-backend-openrouter! :model m)))
  (eng:apply-backend-defaults))

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

(defun run-topup (args)
  "operandi topup ppq AMOUNT [cents|sats] [--to NIP05] [--no-wait]

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
      (format *error-output* "usage: operandi topup ppq AMOUNT [cents|sats] [--to NIP05] [--no-wait]   (AMOUNT: an integer; 1500 cents = $15.00)~%")
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
        (format *error-output* "~&operandi topup: ~A~%" e)
        (sb-ext:exit :code 1)))))

;; May this machine ask its operator for credit? (config auto_topup_cents)
(when (auto-topup-plan)
  (setf eng:*payment-required-hook* #'auto-topup))

;; Apply the configured default model unless the command line names a backend
;; explicitly (--openrouter, with or without a model; a bare --openrouter also
;; reads the config, below). Doing both would announce the backend twice.
(let ((m (and (not (member-if (lambda (a) (member a '("--openrouter" "--routstr" "--ppq") :test #'string=))
                                 (uiop:command-line-arguments)))
              (config-default-model))))
  (when m (use-model! m)))

(let* ((raw (remove "--" (uiop:command-line-arguments) :test #'string=))
       (resume nil)
       (tui-p nil)
       (tool-names nil)        ; set after MCP servers start: --tools/--no-tools may name theirs
       (tools-allow nil)       ; --tools CSV, resolved then
       (tools-deny nil)        ; --no-tools CSV, resolved then
       (mcp-default t)         ; read ~/.operandi/mcp.json (--no-default-mcp: don't)
       (mcp-configs '())       ; --mcp-config FILE, in order
       (mcp-specs '())         ; --mcp NAME=SPEC, in order
       ;; Strip leading flags (--openrouter [model], --resume [id], --tools/--no-tools) in any order.
       (args (let ((acc raw))
               (loop
                 (cond
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
                    (let* ((tail (rest acc)) (next (first tail))
                           (is-model (and next (find #\/ next) (not (find #\Space next)))))
                      (cond (is-model (llm:set-backend-routstr! :model next) (setf acc (rest tail)))
                            (t (llm:set-backend-routstr!) (setf acc tail)))
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
                   ((and acc (string= (first acc) "--confidential"))
                    ;; Require that the exchange be attested. This is a
                    ;; session-wide claim, not a backend: it is checked at
                    ;; startup (below) and again on every request, and it
                    ;; FAILS rather than downgrading to plaintext.
                    (setf acc (rest acc))
                    (llm:enable-confidential!))
                   ;; --model M: M on the CONFIGURED backend (config.json `backend`),
                   ;; so a harness need not know whether this machine pays
                   ;; OpenRouter or PPQ.
                   ((and acc (string= (first acc) "--model"))
                    (unless (and (second acc) (plusp (length (second acc))))
                      (format *error-output* "operandi: --model takes a model id~%")
                      (sb-ext:exit :code 2))
                    (use-model! (second acc))
                    (setf acc (cddr acc)))
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
                   ;; External MCP servers: --mcp-config FILE (repeatable, after the
                   ;; default ~/.operandi/mcp.json), --mcp NAME=SPEC (repeatable, last
                   ;; word), --no-default-mcp (skip the default file).
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
                   ((and acc (string= (first acc) "--tui"))
                    (setf tui-p t acc (rest acc)))
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
                   ((and acc (> (length (first acc)) 2) (string= "--" (first acc) :end2 2)
                         (not (string= (first acc) "--help")))
                    (format *error-output* "operandi: unknown option ~A (try --help)~%" (first acc))
                    (sb-ext:exit :code 2))
                   (t (return))))
               acc))
       ;; --tui and no argument at all are the same thing: the interactive TUI.
       (cmd (if tui-p "tui" (first args))))
  ;; Preflight the model on any path that will actually run a turn (not bare
  ;; help, not the `mcp` listing).
  (unless (member cmd '("--help" "mcp" "topup") :test #'equal)
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
  (unless (member cmd '("--help" "topup") :test #'equal)
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
    ;; operandi mcp — what is configured, what started, which tools it gave us
    ;; operandi topup ppq AMOUNT [cents|sats] [--to NIP05] [--no-wait]
    ((equal cmd "topup") (run-topup (rest args)))
    ((equal cmd "mcp")
     (write-string (operandi.mcp:status-text))
     (finish-output))
    ;; --resume alone or with an explicit tui command → resumed interactive TUI
    ((and resume (or (null cmd) (member cmd '("tui" "shell" "repl") :test #'string-equal)))
     (run-shell resume))
    ;; --resume + a task → continue that session with one more (non-interactive) turn
    (resume (run-resumed-task resume (format nil "~{~A~^ ~}" args)))
    ;; Bare `operandi` (no task, no command) → the interactive TUI.
    ((null cmd) (run-shell))
    ((string= cmd "--help")
     (format t "~&usage:~%")
     (format t "  operandi.lisp -- \"task description\"~%")
     (format t "  operandi.lisp -- ask \"question\"   (rival hypotheses -> swarm verdicts -> answer)~%")
     (format t "  operandi.lisp -- mcp   (list the external MCP servers, their state and tools)~%")
     (format t "  operandi.lisp -- topup ppq AMOUNT [cents|sats] [--to NIP05] [--no-wait]   (buy PPQ credit over Lightning, AMOUNT in base units; the invoice is DMed to NIP05 / config operator_nip05)~%")
     (format t "  operandi.lisp -- --model M \"task\"   (model M on the configured backend: config.json \"backend\": \"ppq\" or \"openrouter\")~%")
     (format t "  operandi.lisp -- --mcp-config FILE \"task\"   (also read FILE's mcpServers; repeatable)~%")
     (format t "  operandi.lisp -- --mcp NAME=COMMAND [ARGS…] \"task\"   (a stdio MCP server; repeatable)~%")
     (format t "  operandi.lisp -- --mcp NAME=https://URL \"task\"   (a Streamable HTTP MCP server)~%")
     (format t "  operandi.lisp -- --no-default-mcp \"task\"   (do not read ~~/.operandi/mcp.json)~%")
     (format t "  operandi.lisp -- ask-json < req.json   (the same as JSON, on one @@OPERANDI-JSON@@ line; for harnesses)~%")
     (format t "  operandi.lisp              (no args, or --tui: interactive REPL: streaming, tools, /commands)~%")
     (format t "  operandi.lisp -- --resume [ID] tui  (resume a saved session; latest if no ID)~%")
     (format t "  operandi.lisp -- --rc TARGET \"task\"   (mirror each turn to a Marmot chat: TARGET is an npub, nprofile or NIP-05 to start a new chat, or a chat name / group id for an existing one; also /rc in the TUI)~%")
     (format t "  operandi.lisp -- --openrouter [MODEL] \"task\"~%")
     (format t "  operandi.lisp -- --effort off|low|medium|high \"task\"   (reasoning; also OPERANDI_EFFORT)~%")
     (format t "  operandi.lisp -- --openrouter STRONG --worker-model CHEAP tui   (orchestrator and subagents on different models)~%")
     (format t "  operandi.lisp -- --tools Read,Write,Edit,Bash,Grep \"task\"   (allow-list)~%")
     (format t "  operandi.lisp -- --no-tools Fan,Task,Spawn \"task\"           (defaults minus these)~%")
     (format t "  bin/operandi-acp                 (ACP server over stdio for editors: Zed, Buzz; see README)~%")
     (format t "~%env: OPERANDI_MAX_TOKENS (per-turn output cap, default 16384),~%")
     (format t "     OPERANDI_CONTEXT_BUDGET (compaction threshold), OPERANDI_MAX_ITERS.~%")
     (format t "  operandi.lisp -- --routstr [MODEL] \"task\"   (Routstr: Cashu token in ~~/.operandi/routstr.token)~%")
     (format t "  operandi.lisp -- --ppq [MODEL] \"task\"~%")
     (format t "      PPQ (ppq.ai), OpenAI-compatible, paid in Bitcoin/Lightning. MODEL is~%")
     (format t "      any PPQ id (gpt-6.1-sol, claude-sonnet-5.5, ...); requests go out in~%")
     (format t "      the clear, like OpenRouter. Key in ~~/.operandi/ppq.token.~%")
     (format t "  operandi.lisp -- --confidential --ppq private/MODEL \"task\"~%")
     (format t "      Path A only: MODEL must be private/*, and every request is EHBP-sealed~%")
     (format t "      to an AMD-attested Tinfoil enclave verified locally against the pinned~%")
     (format t "      AMD ARK. No fallback — an unverifiable enclave or a plaintext answer is~%")
     (format t "      an error, not a downgrade. --confidential on any other backend fails.~%")
     (format t "~%--openrouter reads token from ~~/.operandi/openrouter.token.~%")
     (format t "Sessions are saved under ~~/.operandi/sessions/; --resume continues one.~%")
     (format t "Default backend is a local llama.cpp on http://127.0.0.1:8081.~%"))
    ((member cmd '("tui" "shell" "repl") :test #'string-equal) (run-shell))
    ;; operandi ask "why does X happen?" — the question phase, non-interactively
    ((string-equal cmd "ask")
     (funcall (find-symbol "REPL" "OPERANDI.TUI")
              :once (format nil "/ask ~{~A~^ ~}" (rest args)) :greet nil))
    ;; operandi ask-json < request.json — the question phase as DATA, for a
    ;; harness (the MCP server). Request: {"question": "...", "hypotheses":
    ;; [...]?, "context": "..."?, "synthesize": bool?, "worker_model": "..."?}.
    ;; The result is ONE line on stdout, "@@OPERANDI-JSON@@ {...}", so a
    ;; caller can find it among the progress lines; failures print the same
    ;; line with {"error": "..."} and exit 1.
    ((string-equal cmd "ask-json") (run-ask-json))
    (t (run-once (format nil "~{~A~^ ~}" args) tool-names))))

;; Exit cleanly once the command is done. Without this, an invocation that
;; lacks --non-interactive (e.g. `sbcl --noinform --load bin/operandi.lisp`)
;; drops into SBCL's own REPL after the TUI quits — forcing an extra Ctrl-D.
;; (`bin/operandi` and the MCP already pass --non-interactive; this is harmless
;; there and makes the bare --load form behave the same.)
;; Tear down any per-run Marmot identity (--rc) before exiting.
(ignore-errors (funcall (find-symbol "DISABLE" "OPERANDI.RC")))
(sb-ext:exit :code 0)
