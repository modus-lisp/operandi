;;; bin/operandi.lisp
;;;
;;; CLI for standalone operandi.
;;;
;;; Usage (bin/operandi is the launcher; it dumps a core image and runs this
;;; file from it, so the sbcl invocation below is the slow path, not the way
;;; you normally start operandi):
;;;
;;;   bin/operandi [FLAGS] [COMMAND | "task"]
;;;   bin/operandi --help
;;;
;;;   sbcl --non-interactive --load bin/operandi.lisp -- [FLAGS] [COMMAND | "task"]
;;;
;;; The single-task form runs the agent loop once on the given prompt
;;; and prints the final answer. The shell form is an interactive REPL:
;;; each input line becomes a fresh agent run; type 'exit' to quit.
;;; Conversation history is NOT carried across shell prompts — each task
;;; is isolated.
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
                    (#:llm #:operandi.llm)
                    (#:json #:json-simple)
                    (#:tools #:operandi.tools)))
(in-package #:operandi-cli)

(defun %csv (s) (and s (mapcar (lambda (x) (string-trim " " x)) (uiop:split-string s :separator ","))))
(defun %tools-allow (csv) "The tool set for --tools: exactly the named tools." (%csv csv))
(defun %tools-deny (csv)  "The tool set for --no-tools: the defaults minus the named tools."
  (let ((deny (%csv csv)))
    (remove-if (lambda (n) (member n deny :test #'string-equal)) (tools:default-tools))))

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
   `operandi \"task\" > answer.txt` still gets just the answer."
  (let* ((buf (make-string-output-stream))
         (dim (%stderr-dim-p))
         (think-open nil)
         (think-line (make-string-output-stream))
         (eng:*on-token* (lambda (s) (write-string s buf) (write-string s) (force-output)))
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
      (declare (ignore history))
      (when think-open
        (let ((tail (get-output-stream-string think-line)))
          (when (plusp (length tail))
            (format *error-output* "~@[~C[2m~]│ ~A~@[~C[0m~]~%" (and dim #\Escape) tail (and dim #\Escape)))))
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

(defun %usage ()
  "The --help text.  Written for the LAUNCHER (bin/operandi), which is how
   operandi is actually run; the source form is mentioned once at the end
   rather than prefixed onto every line.  Every flag the parser accepts is
   listed here — a flag that works but is undocumented is a flag nobody
   finds."
  (format t "~&usage: operandi [FLAGS] [COMMAND | \"task\"]~%")
  (format t "~%commands~%")
  (format t "  (none), tui, shell, repl   interactive TUI~%")
  (format t "  \"task description\"         run one task, print the answer, exit~%")
  (format t "  ask \"question\"             the question phase: rival hypotheses, a swarm~%")
  (format t "                             settles them, an answer from the verdicts~%")
  (format t "  --resume [ID] [tui]        resume a saved session (latest if no ID)~%")
  (format t "  --resume [ID] \"task\"       continue that session with one more turn~%")
  (format t "~%backend — default: local llama.cpp at ~A~%" llm:*llama-url*)
  (format t "  --llama ADDR [-m MODEL]    the local llama.cpp server. ADDR is the server's~%")
  (format t "                             address, not a model: [proto://][host][:port][/path],~%")
  (format t "                             e.g. :9931, localhost:9931, http://box:9931. At least~%")
  (format t "                             one component is required, and a port must be~%")
  (format t "                             colon-prefixed. -m MODEL names the model, for a~%")
  (format t "                             multi-model server only; without it the model is~%")
  (format t "                             asked of the server (the only one, else the largest,~%")
  (format t "                             preferring one already loaded).~%")
  (format t "  --openrouter [MODEL]       OpenRouter; MODEL is a vendor/name slug. Bare~%")
  (format t "                             --openrouter uses the configured model. Token in~%")
  (format t "                             ~~/.operandi/openrouter.token.~%")
  (format t "  --routstr [NODE-URL] [MODEL]~%")
  (format t "                             Routstr: independent Lightning-paid nodes. NODE-URL~%")
  (format t "                             (https://...) picks the node; default token in~%")
  (format t "                             ~~/.operandi/routstr.token.~%")
  (format t "  --ppq [MODEL]              PPQ (ppq.ai), OpenAI-compatible, paid in Bitcoin/~%")
  (format t "                             Lightning. Any PPQ id (gpt-6.1-sol, ...); requests~%")
  (format t "                             go out in the clear. Key in ~~/.operandi/ppq.token.~%")
  (format t "  --tinfoil [MODEL]          Tinfoil's own enclave (inference.tinfoil.sh), sealed~%")
  (format t "                             end to end — there is no plaintext mode. MODEL is a~%")
  (format t "                             bare Tinfoil id (gpt-oss-120b, ...). Key in~%")
  (format t "                             ~~/.operandi/tinfoil.token.~%")
  (format t "  --confidential-tinfoil     Require every request to be EHBP-sealed to a Tinfoil~%")
  (format t "                             enclave whose key is bound into an attestation report~%")
  (format t "                             verified locally against the pinned AMD ARK. A claim~%")
  (format t "                             about the SEAL, not the whole exchange. Fails rather~%")
  (format t "                             than downgrading. Valid with --ppq private/MODEL,~%")
  (format t "                             --tinfoil, and --routstr tinfoil-MODEL; on any other~%")
  (format t "                             backend it is a configuration error.~%")
  (format t "~%session~%")
  (format t "  --rc TARGET                mirror each turn to a Marmot chat: an npub, nprofile~%")
  (format t "                             or NIP-05 starts a new chat, a chat name or group id~%")
  (format t "                             joins an existing one. Also /rc in the TUI.~%")
  (format t "  --tui                      force the interactive TUI~%")
  (format t "                             (in the TUI, /help lists the /commands: /model,~%")
  (format t "                             /effort, /workers, /sessions, /resume, /cost,~%")
  (format t "                             /search, /paste, /swarm, /rc, /system, /tools,~%")
  (format t "                             /clear, /ask, /quit)~%")
  (format t "~%agent~%")
  (format t "  --worker-model M           model for sub-agents: a vendor/name slug, llama, or~%")
  (format t "                             inherit (the orchestrator's model)~%")
  (format t "  --effort LEVEL             reasoning effort: off, low, medium, high, default~%")
  (format t "  --tools A,B,C              allow-list: exactly these tools~%")
  (format t "  --no-tools X,Y             the default tool set minus these~%")
  (format t "  --help, -h                 this text~%")
  (format t "  --confidential             REMOVED — renamed to --confidential-tinfoil; the~%")
  (format t "                             old name overclaimed, so it errors rather than~%")
  (format t "                             being accepted silently~%")
  (format t "~%An unknown --flag is rejected (exit 2) rather than sent to the agent as task~%")
  (format t "text. Before any path that runs a turn the model is preflighted, so a bad slug~%")
  (format t "or an unreachable server fails here instead of blank-per-turn.~%")
  (format t "~%env (read when the image loads — on the fast path that means the core build,~%")
  (format t "so set these before the first run, or prefer the flags and /commands)~%")
  (format t "  OPERANDI_MAX_TOKENS        per-turn output cap (default 16384)~%")
  (format t "  OPERANDI_CONTEXT_BUDGET    compaction threshold in tokens~%")
  (format t "  OPERANDI_CONTEXT_HEADROOM  fraction of the budget kept free (default 0.2)~%")
  (format t "  OPERANDI_MAX_ITERS         hard cap on the agentic loop (default 500)~%")
  (format t "  OPERANDI_EFFORT            same as --effort~%")
  (format t "  OPERANDI_WORKER_MODEL      same as --worker-model~%")
  (format t "  OPERANDI_SEARCH            web-search backend: openrouter, searxng, brave, auto~%")
  (format t "  OPERANDI_SWARM_DEADLINE    seconds a Fan/Investigate batch may run (default 900)~%")
  (format t "  OPERANDI_NO_PREFLIGHT=1    skip the model preflight~%")
  (format t "  OPERANDI_FANCY_TUI=1       opt in to the scroll-region TUI~%")
  (format t "  OPERANDI_SBCL              the sbcl binary the launcher uses~%")
  (format t "~%files~%")
  (format t "  ~~/.operandi/config.json    default model: {\"model\": \"vendor/name\"} or \"llama\"~%")
  (format t "  ~~/.operandi/operandi.db    saved sessions; --resume continues one~%")
  (format t "  ~~/.operandi/offload/       raw conversation displaced by compaction~%")
  (format t "~%entry points~%")
  (format t "  bin/operandi               this CLI (dumps a core image for a ~~20ms start)~%")
  (format t "  bin/operandi-acp           ACP server over stdio, for editors (Zed, Buzz)~%")
  (format t "  bin/operandi-nostr.lisp    Nostr DM front end~%")
  (format t "  bin/operandi-sandbox-macos the same CLI under the macOS sandbox profile~%")
  (format t "~%From source, without the launcher:~%")
  (format t "  sbcl --non-interactive --load bin/operandi.lisp -- [FLAGS] [COMMAND | \"task\"]~%"))

;; Apply the configured default model unless the command line names a backend
;; explicitly (--openrouter, with or without a model; a bare --openrouter also
;; reads the config, below). Doing both would announce the backend twice.
(let ((m (and (not (member-if (lambda (a) (member a '("--llama" "--openrouter" "--routstr" "--ppq" "--tinfoil") :test #'string=))
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
       (tui-p nil)
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
                    ;; default.  Under --confidential-tinfoil the backend refuses
                    ;; anything that is not a tinfoil-* model, so a typo cannot
                    ;; silently become a plaintext request to a node that
                    ;; terminates it itself.
                    (let* ((tail (rest acc))
                           (node (and (first tail)
                                      (uiop:string-prefix-p "http" (first tail))
                                      (first tail)))
                           (tail (if node (rest tail) tail))
                           (next (first tail))
                           (is-model (and next (not (uiop:string-prefix-p "--" next))
                                          (or (find #\/ next) (rest tail)))))
                      (let ((ok (if is-model
                                    (llm:set-backend-routstr! :model next :url (or node llm:*routstr-url*))
                                    (llm:set-backend-routstr! :url (or node llm:*routstr-url*)))))
                        ;; A refused switch (no key, or a non-tinfoil model
                        ;; under --confidential-tinfoil) must not fall through to
                        ;; whatever backend was configured before — that is
                        ;; exactly the silent downgrade this flag exists to
                        ;; prevent.
                        (unless ok (sb-ext:exit :code 2))
                        (setf acc (if is-model (rest tail) tail)))
                      (eng:apply-backend-defaults)))
                   ((and acc (string= (first acc) "--ppq"))
                    ;; PPQ, in one of two modes. A model here is any PPQ id
                    ;; (gpt-6.1-sol, private/gpt-oss-120b, ...); a bare flag
                    ;; takes the library default. Under --confidential-tinfoil the
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
                        ;; under --confidential-tinfoil) must not fall through to
                        ;; whatever backend was configured before — that is
                        ;; exactly the silent downgrade this flag exists to
                        ;; prevent.
                        (unless ok (sb-ext:exit :code 2))
                        (setf acc (if is-model (rest tail) tail)))
                      (eng:apply-backend-defaults)))
                   ((and acc (string= (first acc) "--tinfoil"))
                    ;; Tinfoil's OWN enclave (inference.tinfoil.sh), sealed end
                    ;; to end. There is only one path here, so there is no
                    ;; --confidential-tinfoil to ask for: every request is EHBP-sealed
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
                           (is-model (and next (not (uiop:string-prefix-p "--" next))
                                          (rest tail))))
                      (let ((ok (if is-model
                                    (llm:set-backend-tinfoil! :model next)
                                    (llm:set-backend-tinfoil!))))
                        (unless ok (sb-ext:exit :code 2))
                        (setf acc (if is-model (rest tail) tail)))
                      (eng:apply-backend-defaults)))
                   ((and acc (string= (first acc) "--confidential-tinfoil"))
                    ;; Require that the exchange be sealed to a Tinfoil enclave.
                    ;; The name is deliberately narrow: it does NOT claim the
                    ;; request is confidential in general, only that it is
                    ;; EHBP-sealed to a key bound into a Tinfoil attestation
                    ;; report we verify locally.  This is a session-wide claim,
                    ;; not a backend: it is checked at startup (below) and again
                    ;; on every request, and it FAILS rather than downgrading to
                    ;; plaintext.
                    (setf acc (rest acc))
                    ;; A backend already chosen may not be able to honour it
                    ;; (e.g. --routstr with a model the node terminates itself).
                    ;; That is a configuration error, so say so and stop — do
                    ;; not fall through to a plaintext session.
                    (handler-case (llm:enable-confidential!)
                      (error (e)
                        (format *error-output* "~&operandi: ~A~%" e)
                        (sb-ext:exit :code 2))))
                   ((and acc (string= (first acc) "--confidential"))
                    ;; The old name.  It overclaimed — it read as a promise
                    ;; about the whole exchange when all it ever meant was
                    ;; "sealed to a Tinfoil enclave" — so it is not accepted
                    ;; silently.  Say what it became and stop.
                    (format *error-output*
                            "~&operandi: --confidential was renamed to --confidential-tinfoil.~%~
                             ~&  It only ever meant \"sealed to a Tinfoil enclave\", and the old~%~
                             ~&  name read as a broader promise than that.~%")
                    (sb-ext:exit :code 2))
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
                    (setf tool-names (%tools-allow (second acc)) acc (cddr acc)))
                   ((and acc (string= (first acc) "--no-tools"))
                    (setf tool-names (%tools-deny (second acc)) acc (cddr acc)))
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
       ;; -h is --help: it is short enough to slip past the unknown-option
       ;; check below, so without this it would be handed to the agent as a
       ;; one-word task.
       (cmd (let ((c (if tui-p "tui" (first args))))
              (if (and c (string= c "-h")) "--help" c))))
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
  (when (and (eq llm:*llm-backend* :llama) (not (equal cmd "--help")))
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
  ;; Preflight the model on any path that will actually run a turn (not bare help).
  (unless (equal cmd "--help")
    ;; A confidential session on a backend that cannot seal is a configuration
    ;; error, and it must be fatal BEFORE the first turn — not a warning that
    ;; scrolls past while plaintext goes out.
    (handler-case (llm:require-confidential!)
      (error (e)
        (format *error-output* "~&operandi: ~A~%" e)
        (sb-ext:exit :code 2)))
    (preflight-or-die))
  (cond
    ;; --resume alone or with an explicit tui command → resumed interactive TUI
    ((and resume (or (null cmd) (member cmd '("tui" "shell" "repl") :test #'string-equal)))
     (run-shell resume))
    ;; --resume + a task → continue that session with one more (non-interactive) turn
    (resume (run-resumed-task resume (format nil "~{~A~^ ~}" args)))
    ;; Bare `operandi` (no task, no command) → the interactive TUI.
    ((null cmd) (run-shell))
    ((string= cmd "--help") (%usage))
    ((member cmd '("tui" "shell" "repl") :test #'string-equal) (run-shell))
    ;; operandi ask "why does X happen?" — the question phase, non-interactively
    ((string-equal cmd "ask")
     (funcall (find-symbol "REPL" "OPERANDI.TUI")
              :once (format nil "/ask ~{~A~^ ~}" (rest args)) :greet nil))
    (t (run-once (format nil "~{~A~^ ~}" args) tool-names))))

;; Exit cleanly once the command is done. Without this, an invocation that
;; lacks --non-interactive (e.g. `sbcl --noinform --load bin/operandi.lisp`)
;; drops into SBCL's own REPL after the TUI quits — forcing an extra Ctrl-D.
;; (`bin/operandi` and the MCP already pass --non-interactive; this is harmless
;; there and makes the bare --load form behave the same.)
;; Tear down any per-run Marmot identity (--rc) before exiting.
(ignore-errors (funcall (find-symbol "DISABLE" "OPERANDI.RC")))
(sb-ext:exit :code 0)
