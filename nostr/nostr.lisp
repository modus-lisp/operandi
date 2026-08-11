;;;; nostr/nostr.lisp  (operandi.nostr)  — the `operandi/nostr` secondary system
;;;;
;;;; A HEADLESS operandi agent that talks to its operator over Nostr private DMs
;;;; (NIP-17 gift-wrapped kind-14 chat). The operator is identified by a NIP-05
;;;; address (name@domain), resolved to a pubkey at startup. Every inbound gift
;;;; wrap is unwrapped, the sender is cryptographically authenticated (the key
;;;; that signed the seal), and ONLY the operator is answered — the reply is
;;;; gift-wrapped back so only the operator can read it. Conversation history is
;;;; threaded so it's a real running chat (the engine auto-compacts as it grows).
;;;;
;;;; All Nostr mechanics come from cl-nostr (keys/event/filter/pool/nip05/nip59 —
;;;; nip59 build-giftwrap/unwrap-giftwrap IS the NIP-17 core); the brain is
;;;; operandi.engine:run. This system is OPTIONAL: the core :operandi stays
;;;; dependency-light, and :operandi/nostr pulls cl-nostr for those who want it.

(defpackage #:operandi.nostr
  (:use #:cl)
  (:local-nicknames (#:eng    #:operandi.engine)
                    (#:llm    #:operandi.llm)
                    (#:tools  #:operandi.tools)
                    (#:keys   #:cl-nostr.keys)
                    (#:ev     #:cl-nostr.event)
                    (#:flt    #:cl-nostr.filter)
                    (#:pool   #:cl-nostr.pool)
                    (#:b32    #:cl-nostr.bech32)
                    (#:nip05  #:cl-nostr.nip05)
                    (#:nip59  #:cl-nostr.nip59)
                    (#:bt     #:bordeaux-threads))
  (:export #:*owner-nip05* #:*relays* #:*system-prompt* #:*tool-names* #:*model*
           #:*key-file* #:*state-file* #:*max-reply-chars* #:*greeting*
           #:agent-npub #:agent-pubkey #:start #:stop #:running-p #:run-loop
           #:process-item #:send-dm))
(in-package #:operandi.nostr)

;;; ------------------------------- config -------------------------------
(defparameter *owner-nip05* nil
  "NIP-05 address of the operator (name@domain). REQUIRED — resolved to a pubkey
   at startup; only DMs from that key are answered. Set before START or pass :nip05.")
(defparameter *relays*
  '("wss://relay.damus.io" "wss://nos.lol" "wss://relay.primal.net")
  "Default DM relays the agent listens on + publishes replies to. Unioned at
   startup with the operator's advertised DM relays (kind 10050) and NIP-05 relays.")
(defparameter *model* "deepseek/deepseek-v4-flash"
  "OpenRouter model the DM agent runs on. When non-NIL, START switches operandi.llm
   to this OpenRouter model; NIL leaves the LLM backend as the caller configured it.")
(defparameter *tool-names* nil
  "Tool allow-list for the engine; NIL = the engine default (full toolset). The
   sender is cryptographically authenticated as the operator, so full tools is
   reasonable for a personal agent — restrict here for a read-only DM bot.")
(defparameter *system-prompt*
  "You are operandi, replying to your operator over a private Nostr DM. This is a
phone-style chat: answer concisely in plain text (no markdown tables, no code
fences unless asked). Lead with the answer. You are a reasoner over data and a
capable coding agent — use your tools to look things up and get things done
before answering. If something is ambiguous or you couldn't find it, say so
plainly; never fabricate a value to fill a hole."
  "Persona/system prompt for DM turns.")

(defparameter +giftwrap-kind+ 1059)
(defparameter +dm-relays-kind+ 10050)   ; NIP-17: a user's DM inbox relay list
(defparameter *key-file*
  (merge-pathnames ".operandi/nostr-agent.key" (user-homedir-pathname)))
(defparameter *state-file*
  (merge-pathnames ".operandi/nostr-agent.state" (user-homedir-pathname)))
(defparameter *transcript-file*
  (merge-pathnames ".operandi/nostr-agent.transcript" (user-homedir-pathname))
  "Human-readable DM transcript: every inbound operator message and every reply
   the agent sends, timestamped. NIL disables. Also echoed to stdout (the log).")
(defparameter *max-reply-chars* 3500)
(defparameter *greeting* "operandi is online and listening — DM me anytime."
  "One-line DM sent to the operator on startup when START is called with :greet t,
   so you know the agent came up.")

;;; --------------------------- agent identity ---------------------------
(defvar *agent-kp* nil "the agent's cl-nostr keypair.")
(defun ensure-agent-key ()
  "Load the agent's persistent keypair, or mint + persist one on first run."
  (or *agent-kp*
      (setf *agent-kp*
            (if (probe-file *key-file*)
                (keys:keypair-from-secret
                 (with-open-file (s *key-file*) (string-trim '(#\Space #\Newline) (read-line s))))
                (let ((kp (keys:generate-keypair)))
                  (ignore-errors
                    (ensure-directories-exist *key-file*)
                    (with-open-file (s *key-file* :direction :output
                                       :if-exists :supersede :if-does-not-exist :create)
                      (format s "~a~%" (keys:secret-hex kp)))
                    #+sbcl (sb-posix:chmod (namestring *key-file*) #o600))
                  kp)))))
(defun agent-pubkey () (keys:public-hex (ensure-agent-key)))
(defun agent-npub () (b32:npub-encode (agent-pubkey)))

;;; ------------------------------- state --------------------------------
(defvar *owner-pubkey* nil)
(defvar *pool* nil)
(defvar *floor* 0 "FIXED at startup: skip anything older (the pre-startup backlog /
   messages from while we were down). Does NOT advance during a session, so
   out-of-order live gift-wraps aren't skipped — dedup is by event id (*seen*).")
(defvar *watermark* 0 "highest RUMOR created_at answered; persisted so the next
   restart's *floor* skips what we already handled. Advances, but is NOT the live
   admission test (that's *floor* + *seen*).")
(defvar *seen* (make-hash-table :test 'equal) "processed gift-wrap event ids.")
(defvar *history* nil "threaded conversation history (engine message list).")
(defvar *queue* nil) (defvar *qlock* (bt:make-lock "nostr-q")) (defvar *qcv* (bt:make-condition-variable))
(defvar *running* nil) (defvar *worker* nil)
(defun running-p () *running*)

(defun log-msg (arrow text)
  "Append one ARROW-prefixed transcript line (timestamped) to the transcript file
   and echo it to stdout (captured in the nohup log). ARROW is \"<\" (inbound) or
   \">\" (reply). Never signals."
  (ignore-errors
    (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time))
      (let ((line (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d  ~a ~a"
                          y mo d h mi s arrow
                          (substitute #\Space #\Newline text))))
        (format t "~&~a~%" line) (force-output)
        (when *transcript-file*
          (ensure-directories-exist *transcript-file*)
          (with-open-file (o *transcript-file* :direction :output
                             :if-exists :append :if-does-not-exist :create :external-format :utf-8)
            (write-line line o)))))))

(defun read-watermark ()
  (ignore-errors
    (when (probe-file *state-file*)
      (with-open-file (s *state-file*) (let ((v (read s nil nil))) (and (integerp v) v))))))
(defun write-watermark (ts)
  (ignore-errors
    (ensure-directories-exist *state-file*)
    (with-open-file (s *state-file* :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s "~a~%" ts))))

;;; ---------------------------- relay discovery -------------------------
(defun owner-dm-relays ()
  "The operator's advertised NIP-17 DM inbox relays (kind 10050 'relay' tags), so
   our replies reach a relay they actually read. Best-effort."
  (ignore-errors
    (let ((evs (pool:fetch-events *pool*
                (list (flt:make-filter :kinds (list +dm-relays-kind+)
                                       :authors (list *owner-pubkey*) :limit 1))
                :timeout 5)))
      (when evs (ev:tag-values (first evs) "relay")))))

;;; ------------------------------- brain --------------------------------
(defun answer (text)
  "Run one operandi turn on TEXT (threading history), return the reply string.
   NOTE: eng:run IGNORES its prompt arg when :history is non-NIL — the caller must
   append the new user message to the history itself (same as the TUI). Passing
   *history* alone would drop this message and the model would answer the PREVIOUS
   turn (the 'Going well! How about you?' bug)."
  (multiple-value-bind (reply hist)
      (eng:run text
               :history (when *history*
                          (append *history* (list (llm:ht "role" "user" "content" text))))
               :system *system-prompt*
               :tool-names (or *tool-names* (tools:default-tools))
               :verbose nil)
    (setf *history* hist)
    (let ((s (string-trim '(#\Space #\Newline #\Return #\Tab) (or reply ""))))
      (when (zerop (length s)) (setf s "(no reply produced)"))
      (if (> (length s) *max-reply-chars*)
          (concatenate 'string (subseq s 0 *max-reply-chars*) " …")
          s))))

(defun send-dm (text)
  "Gift-wrap TEXT to the operator (NIP-17) and publish it to the DM relays."
  (let ((wrap (nip59:build-giftwrap (keys:keypair-secret-key (ensure-agent-key))
                                    *owner-pubkey* text)))
    (pool:pool-publish *pool* wrap)))

;;; --------------------------- receive + dispatch -----------------------
(defun on-giftwrap (event relay)
  "Relay-thread callback: unwrap, authenticate the sender, and enqueue an operator
   message newer than the watermark. Cheap work only; the LLM turn runs on the
   worker thread. Never signals."
  (declare (ignore relay))
  (let ((id (ev:event-id event)))
    (unless (gethash id *seen*)
      (setf (gethash id *seen*) t)          ; dedup by id — set BEFORE unwrap
      (multiple-value-bind (text sender created)
          ;; a wrap we can't decrypt isn't ours (spam / mistargeted #p) — ignore quietly
          (ignore-errors (nip59:unwrap-giftwrap (keys:keypair-secret-key (ensure-agent-key)) event))
        (when (and (equal sender *owner-pubkey*)
                   (integerp created) (> created *floor*)   ; FIXED floor, not advancing watermark
                   (stringp text) (plusp (length (string-trim " " text))))
          (log-msg "< operator:" text)
          (bt:with-lock-held (*qlock*)
            (setf *queue* (nconc *queue* (list (list created text))))
            (bt:condition-notify *qcv*)))))))

(defun process-item (created text)
  "Answer one operator message and send the reply; advance the watermark. Never
   signals (a brain/send failure becomes an apology DM / a logged line)."
  (let ((reply (handler-case (answer text)
                 (serious-condition (e) (format nil "Sorry — I hit an error: ~A" e)))))
    (log-msg "> operandi:" reply)
    (handler-case (send-dm reply)
      (serious-condition (e)
        (format *error-output* "~&[operandi.nostr] send failed: ~A~%" e)))
    (when (> created *watermark*) (setf *watermark* created) (write-watermark created))
    reply))

(defun worker-loop ()
  (loop while *running* do
    (let ((item (bt:with-lock-held (*qlock*)
                  (loop until (or (not *running*) *queue*)
                        do (bt:condition-wait *qcv* *qlock*))
                  (and *queue* (pop *queue*)))))
      (when item (destructuring-bind (created text) item (process-item created text))))))

;;; ------------------------------- lifecycle ----------------------------
(defun start (&key (nip05 *owner-nip05*) (model *model*) (announce t) (publish-meta t) (greet nil))
  "Resolve the operator's NIP-05, connect the relay pool, (optionally) advertise
   our DM relays, and start answering the operator's NIP-17 DMs. Returns after
   wiring up the subscription + worker; use RUN-LOOP to block the process."
  (when *running* (return-from start :already-running))
  (unless nip05 (error "operandi.nostr: set *owner-nip05* (or pass :nip05) — the operator's NIP-05 address"))
  (ensure-agent-key)
  (when model (llm:use-openrouter :model model))
  (setf *owner-pubkey* (nip05:resolve-pubkey nip05))
  (unless *owner-pubkey* (error "could not resolve NIP-05 ~A to a pubkey" nip05))
  (let* ((owner-relays (ignore-errors (nth-value 1 (nip05:resolve nip05))))
         (relays (remove-duplicates (append *relays* owner-relays) :test #'equal)))
    (setf *pool* (pool:make-pool relays))
    ;; publish our own kind-10050 so the operator's client sends DMs to relays we read
    (when publish-meta
      (ignore-errors
        (pool:pool-publish *pool*
          (ev:build-event (ensure-agent-key) +dm-relays-kind+ ""
                          :tags (mapcar (lambda (u) (list "relay" u)) relays)))))
    ;; add the operator's advertised DM relays too (so replies reach them)
    (dolist (u (owner-dm-relays)) (ignore-errors (pool:add-relay *pool* u)))
    (setf *floor* (or (read-watermark)
                      (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))
          *watermark* *floor*                 ; advances from here; *floor* stays put
          (gethash :init *seen*) t
          *running* t
          *worker* (bt:make-thread #'worker-loop :name "operandi-nostr-worker"))
    (pool:pool-subscribe *pool*
      (list (flt:make-filter :kinds (list +giftwrap-kind+)
                             :tags (list (cons "p" (list (agent-pubkey))))))
      :on-event #'on-giftwrap)
    (when announce
      (format t "~&[operandi.nostr] live over NIP-17.~%  agent npub: ~A~%  operator:   ~A (~A)~%  relays:     ~{~A~^ ~}~%  model:      ~A~%"
              (agent-npub) nip05 (b32:npub-encode *owner-pubkey*) relays (or model "(caller-configured)"))
      (force-output))
    ;; greet the operator so they know it came up (best-effort; never blocks start)
    (when greet (ignore-errors (send-dm *greeting*))))
  :started)

(defun stop ()
  (setf *running* nil)
  (bt:with-lock-held (*qlock*) (bt:condition-notify *qcv*))
  (ignore-errors (when *pool* (pool:close-pool *pool*)))
  (ignore-errors (when (and *worker* (bt:thread-alive-p *worker*)) (bt:join-thread *worker* :timeout 6)))
  (setf *worker* nil *pool* nil)
  :stopped)

(defun run-loop (&rest start-args)
  "START, then block until Ctrl-C / STOP. The headless entry point."
  (apply #'start start-args)
  (handler-case (loop (sleep 3600))
    (#+sbcl sb-sys:interactive-interrupt #-sbcl error () (stop) (format t "~&[operandi.nostr] stopped.~%"))))
