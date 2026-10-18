;;; src/acp.lisp  (operandi.acp)
;;;
;;; An Agent Client Protocol (agentclientprotocol.com) server: lets editors
;;; like Zed drive operandi as a coding agent over JSON-RPC 2.0 on stdio
;;; (newline-delimited). It's an ADAPTER — the agent work is done; this maps
;;; operandi's existing seams onto the wire:
;;;
;;;   eng:*on-token*         -> session/update  agent_message_chunk
;;;   hooks pre/post         -> session/update  tool_call / tool_call_update
;;;   the TodoWrite plan     -> session/update  plan
;;;   the usage struct       -> session/update  usage_update
;;;   operandi.session       -> session/new + session/load (resume)
;;;   tools:*tool-gate*      -> session/request_permission (ask the editor)
;;;
;;; CONCURRENCY IS ACTORS (operandi.actor), sharing nothing:
;;;   the ROUTER (the thread that called SERVE) owns stdout, the sessionId ->
;;;     session-actor map, the outbound-request map and the store.  Everything
;;;     that reaches the wire passes through it as a message, in order.
;;;   a READER actor turns stdin lines into messages to the router.
;;;   one SESSION actor per session owns its history and runs its turns; a turn
;;;     that asks the client for permission waits for the router to forward the
;;;     reply, and checks for session/cancel at every streamed token, tool call
;;;     and permission wait.
;;; With no router (*ROUTER* NIL: the handler oracle, inspect/acp-test.lisp)
;;; the same functions run inline on the caller's thread.
;;;
;;; Entry: (operandi.acp:serve).  Everything incidental (engine/tool prints,
;;; warnings) is routed to *error-output* so only JSON-RPC touches stdout.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  ;; #+QUICKLISP: the .asd already loads these; this is for loading the file by hand.  Guarded
  ;; because READING `ql:' is an error in an image without Quicklisp, before anything runs.
  #+quicklisp (ql:quickload '(:json-simple :uiop) :silent t))

(defpackage #:operandi.acp
  (:use #:cl)
  (:local-nicknames (#:eng     #:operandi.engine)
                    (#:llm     #:operandi.llm)
                    (#:tools   #:operandi.tools)
                    (#:hooks   #:operandi.hooks)
                    (#:session #:operandi.session)
                    (#:store   #:operandi.store)
                    (#:actor   #:operandi.actor)
                    (#:json    #:json-simple))
  (:export #:serve #:*protocol-version*))

(in-package #:operandi.acp)

(defparameter *protocol-version* 1)
(defparameter +null+ 'cl:null "The json-simple value that serializes to JSON null.")

;;; ------------------------------ transport ---------------------------

(defvar *out* nil "Protocol output stream (real stdout).  Only the router writes it.")
(defvar *log* nil "Where incidental output goes (stderr).")
(defvar *router* nil
  "The router actor while SERVE runs, else NIL.  Set globally (not bound) so
   every actor sees it.")
(defvar *outq* nil
  "With no router: FIFO of output lines, drained by the handler oracle.")
(defvar *rpc-seq* 0 "Per session actor: outbound request counter.")
(defvar *sid* nil "Current sessionId during a prompt turn.")
(defvar *msg-id* nil)
(defvar *msg-seq* 0)
(defvar *pending* (make-hash-table :test 'equal)
  "Router: outbound request id -> the actor waiting for the client's reply.")

(defun obj (&rest pairs)
  "String-keyed hash-table for json-simple, OMITTING any key whose value is :SKIP."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          unless (eq v :skip) do (setf (gethash k h) v))
    h))

(defun router-p () (and *router* (eql (actor:self) *router*)))

(defun emit (s)
  "On the router: write line S to the protocol stream.  With no router: queue it."
  (if *router*
      (progn (write-string s *out*) (write-char #\Newline *out*) (force-output *out*))
      (setf *outq* (nconc *outq* (list s)))))

(defun to-router (msg)
  "Hand MSG to the router, or handle it here when this IS the router (or there
   is none).  Never blocks on the wire."
  (if (and *router* (not (router-p)))
      (actor:send *router* msg)
      (router-handle msg)))

(defun send-line (message)
  (to-router (list :out (json:stringify message))))

(defun send-notification (method params)
  (send-line (obj "jsonrpc" "2.0" "method" method "params" params)))
(defun send-response (id result)
  (send-line (obj "jsonrpc" "2.0" "id" id "result" result)))
(defun send-error (id code msg)
  (send-line (obj "jsonrpc" "2.0" "id" id "error" (obj "code" code "message" msg))))

(defun next-rpc-id ()
  "Unique per session: one turn at a time, so sessionId + counter."
  (format nil "~A-~D" (or *sid* "acp") (incf *rpc-seq*)))

(defun check-cancel ()
  "In a turn: end it if session/cancel has arrived."
  (when *router*
    (multiple-value-bind (m got) (actor:receive-if (lambda (m) (eq m :cancel)) 0)
      (declare (ignore m))
      (when got (throw 'acp-cancel :cancelled)))))

;; Outbound request (agent -> client), blocking this turn until the client
;; replies.  The router learns who is waiting BEFORE the request can reach the
;; wire (both go through its mailbox, in order), and forwards the reply.
(defun call-client (method params)
  (let ((id (next-rpc-id)))
    (to-router (list :pending id (actor:self)))
    (send-line (obj "jsonrpc" "2.0" "id" id "method" method "params" params))
    (let ((m (actor:receive-if (lambda (m) (or (eq m :cancel)
                                               (and (consp m) (eq (first m) :reply)
                                                    (equal (second m) id)))))))
      (when (eq m :cancel) (throw 'acp-cancel :cancelled))
      (third m))))

(defun route-response (id msg)
  (let ((who (gethash id *pending*)))
    (when who
      (remhash id *pending*)
      (actor:send who (list :reply id (gethash "result" msg))))))

;;; ------------------------------ sessions ----------------------------

(defvar *sessions* (make-hash-table :test 'equal)
  "sessionId -> plist.  In a session actor (and with no router): its own
   session, (:session :cwd :allow :active :system-prompt).  On the router:
   (:actor id).")
(defun sget (sid) (gethash sid *sessions*))
(defun sput (sid plist) (setf (gethash sid *sessions*) plist))
(defun supdate (sid key val)
  (let ((p (gethash sid *sessions*))) (when p (setf (getf p key) val (gethash sid *sessions*) p))))

;;; ------------------------- content <-> wire -------------------------

(defun text-block (s) (obj "type" "text" "text" (or s "")))

(defun blocks->content (blocks)
  "A prompt's ContentBlock array as message content: the text blocks
   flattened to one string (BLOCKS->TEXT), plus an image_url part per
   image block — ACP hands those over as base64 + mimeType, which is a
   data: URL away from what the chat API wants."
  (let ((text (blocks->text blocks))
        (images (when (vectorp blocks)
                  (loop for b across blocks
                        when (and (hash-table-p b) (equal (gethash "type" b) "image")
                                  (stringp (gethash "data" b)))
                          collect (llm:ht "type" "image_url"
                                          "image_url"
                                          (llm:ht "url" (format nil "data:~A;base64,~A"
                                                                (or (gethash "mimeType" b) "image/png")
                                                                (gethash "data" b))))))))
    (if images
        (coerce (cons (llm:text-part text) images) 'vector)
        text)))

(defun blocks->text (blocks)
  "Flatten a prompt's ContentBlock array to a single user string."
  (with-output-to-string (o)
    (when (vectorp blocks)
      (loop for b across blocks
            when (hash-table-p b)
            do (let ((type (gethash "type" b)))
                 (cond
                   ((equal type "text") (write-string (or (gethash "text" b) "") o))
                   ((equal type "resource")
                    (let ((r (gethash "resource" b)))
                      (when (hash-table-p r)
                        (format o "~%[embedded ~A]~%~A~%"
                                (gethash "uri" r) (or (gethash "text" r) "")))))
                   ((equal type "resource_link")
                    (format o "~A (~A)" (or (gethash "name" b) "") (gethash "uri" b)))
                   ((equal type "image") (format o "[attached image]"))
                   ((equal type "audio") (format o "[audio omitted]"))))))))

(defparameter +tool-kind+
  '(("Read" . "read") ("Grep" . "search") ("Glob" . "search")
    ("Write" . "edit") ("Edit" . "edit")
    ("Bash" . "execute") ("Eval" . "execute")
    ("WebFetch" . "fetch") ("WebSearch" . "fetch")
    ("TodoWrite" . "think")))
(defun tool-kind (name) (or (cdr (assoc name +tool-kind+ :test #'string=)) "other"))

(defun %arg (args key) (and (hash-table-p args) (gethash key args)))
(defun tool-title (name args)
  (let ((a (or (%arg args "path") (%arg args "file_path") (%arg args "command") (%arg args "pattern")
               (%arg args "url") (%arg args "query") (%arg args "form"))))
    (if (stringp a)
        (format nil "~A: ~A" name (if (> (length a) 60) (concatenate 'string (subseq a 0 60) "…") a))
        name)))
(defun tool-locations (name args)
  ;; operandi's own tools name it "path"; "file_path" is kept for tools that
  ;; follow the other convention.
  (let ((p (or (%arg args "path") (%arg args "file_path"))))
    (if (and p (stringp p)) (vector (obj "path" p)) :skip)))

(defun clip (s n)
  (let ((s (if (stringp s) s (princ-to-string (or s "")))))
    (if (> (length s) n) (concatenate 'string (subseq s 0 n) "…") s)))

(defun tool-failed-p (result)
  (and (stringp result)
       (or (eql 0 (search "TOOL ERROR" result)) (eql 0 (search "REFUSED" result)))))

;;; --------------------- per-turn streaming hooks ---------------------

(defvar *tool-id* nil "The current toolCallId (pre/post correlation), bound per turn.")
(defvar *tool-seq* 0)

(defun update! (u) (when *sid* (send-notification "session/update" (obj "sessionId" *sid* "update" u))))

(defun acp-on-token (tok)
  (check-cancel)
  (when (and tok (plusp (length tok)))
    (update! (obj "sessionUpdate" "agent_message_chunk"
                  "messageId" *msg-id* "content" (text-block tok)))))

(defun acp-on-reasoning (tok)
  "The model's scratchpad, as ACP's agent_thought_chunk — the client decides
   whether to show it (Zed renders it collapsed).  Without this a reasoning
   model's commentary around a tool call is invisible to the client."
  (check-cancel)
  (when (and tok (plusp (length tok)))
    (update! (obj "sessionUpdate" "agent_thought_chunk"
                  "messageId" *msg-id* "content" (text-block tok)))))

(defun acp-pre-hook (name args)
  (check-cancel)
  (when *sid*
    (let ((id (setf *tool-id* (format nil "call_~D" (incf *tool-seq*)))))
      (update! (obj "sessionUpdate" "tool_call"
                    "toolCallId" id
                    "title" (tool-title name args)
                    "kind" (tool-kind name)
                    "status" "in_progress"
                    "rawInput" (if (hash-table-p args) args :skip)
                    "locations" (tool-locations name args))))))

(defun acp-post-hook (name args result err ms)
  (declare (ignore args err ms))
  (when *sid*
    (let ((id (shiftf *tool-id* nil)))
      (when id
        (update! (obj "sessionUpdate" "tool_call_update"
                      "toolCallId" id
                      "status" (if (tool-failed-p result) "failed" "completed")
                      "content" (vector (obj "type" "content"
                                             "content" (text-block (clip result 2000)))))))
      (when (string= name "TodoWrite") (send-plan)))))

(defun send-plan ()
  (let ((todos tools:*todos*))
    (when (and *sid* todos)
      (update! (obj "sessionUpdate" "plan"
                    "entries" (coerce (mapcar (lambda (td)
                                                (obj "content" (or (getf td :subject) "")
                                                     "priority" "medium"
                                                     "status" (or (getf td :status) "pending")))
                                              todos)
                                      'vector))))))

(defun send-usage (usage)
  (when (and *sid* usage)
    (let ((used (+ (llm:usage-prompt-tokens usage) (llm:usage-completion-tokens usage))))
      (update! (obj "sessionUpdate" "usage_update"
                    "used" used
                    "size" (max used 200000)
                    "cost" (obj "amount" (llm:usage-cost-usd usage) "currency" "USD"))))))

;;; ------------------------- permission gate --------------------------

(defparameter *ask-kinds* '("edit" "execute" "delete")
  "Tool kinds that require the client's OK before running (via
   session/request_permission). read/search/fetch/other auto-allow.")

(defun request-permission (sid name args tool-id)
  "Ask the client. Returns :allow-once, :allow-always, or :reject."
  (let* ((resp (call-client
                "session/request_permission"
                (obj "sessionId" sid
                     "toolCall" (obj "toolCallId" tool-id
                                     "title" (tool-title name args)
                                     "kind" (tool-kind name)
                                     "status" "pending")
                     "options" (vector (obj "optionId" "allow" "name" "Allow" "kind" "allow_once")
                                       (obj "optionId" "always" "name" "Always allow" "kind" "allow_always")
                                       (obj "optionId" "reject" "name" "Reject" "kind" "reject_once")))))
         ;; The spec nests the decision: {"outcome": {"outcome": "selected",
         ;; "optionId": ...}}.  The flat {"outcome": "selected", "optionId": ...}
         ;; is what this server (and its conformance client) spoke first; a
         ;; spec client's reply read as flat was never "selected", so every
         ;; permission it granted came back REFUSED.  Take either.
         (o (and (hash-table-p resp) (gethash "outcome" resp)))
         (sel (if (hash-table-p o) o resp))
         (outcome (if (hash-table-p o) (gethash "outcome" o) o)))
    (cond
      ((equal outcome "selected")
       (let ((opt (gethash "optionId" sel)))
         (cond ((equal opt "always") :allow-always)
               ((equal opt "reject") :reject)
               (t :allow-once))))
      (t :reject))))

(defun acp-gate (sid name args)
  "tools:*tool-gate*: NIL to allow, or a REFUSED string to deny."
  (let ((kind (tool-kind name)))
    (if (not (member kind *ask-kinds* :test #'string=))
        nil                                    ; read-only-ish: auto-allow
        (let ((p (sget sid)))
          (if (member name (getf p :allow) :test #'string=)
              nil                              ; already always-allowed this session
              (let ((tool-id *tool-id*))
                (case (request-permission sid name args (or tool-id "call"))
                  (:allow-always (supdate sid :allow (cons name (getf p :allow))) nil)
                  (:allow-once nil)
                  (t "REFUSED: the user denied permission for this action."))))))))

;;; ------------------------------- turn -------------------------------

(defun user-msg (text) (llm:ht "role" "user" "content" text))

(defun finish-turn (sess messages r)
  "Persist a completed turn and return its ACP stopReason. R is the
   multiple-value-list of eng:run: (text history iters usage)."
  (destructuring-bind (&optional text hist* iters usage) r
    (declare (ignore iters))
    (setf (session:session-history sess) (or hist* messages))
    (incf (gethash :turns sess))
    (when usage (session:session-add-usage sess usage) (send-usage usage))
    (session:persist-session sess)
    (if (and (stringp text) (eql 0 (search "[max-iterations" text)))
        "max_turn_requests" "end_turn")))

(defun run-prompt-turn (sid req-id prompt-text)
  (let* ((p (sget sid))
         (sess (getf p :session))
         (host-sys (getf p :system-prompt))
         ;; A host (e.g. buzz-acp) may pass a `systemPrompt` on session/new to
         ;; teach the agent its environment. Combine it WITH operandi's own base
         ;; prompt (keep operandi's discipline; add the host's context) — used
         ;; only on the first turn; later turns carry the system msg in history.
         (sys (when (and (stringp host-sys) (plusp (length host-sys)))
                (format nil "~A~%~%~A"
                        (funcall (find-symbol "BUILD-SYSTEM-PROMPT" "OPERANDI.ENGINE"))
                        host-sys)))
         (hist (session:session-history sess))
         (messages (if hist (append hist (list (user-msg prompt-text))) nil))
         (*sid* sid)
         (*msg-id* (format nil "msg_~D" (incf *msg-seq*)))
         (*tool-id* nil)
         (*standard-output* *log*)                          ; keep the protocol channel clean
         (hooks:*pre-tool-hooks*  (cons #'acp-pre-hook hooks:*pre-tool-hooks*))
         (hooks:*post-tool-hooks* (cons #'acp-post-hook hooks:*post-tool-hooks*))
         (tools:*tool-gate* (lambda (name args) (acp-gate sid name args)))
         (eng:*stream* t)
         (eng:*on-token* #'acp-on-token)
         (eng:*on-reasoning* #'acp-on-reasoning)
         (stop "end_turn"))
    (supdate sid :active t)
    (unwind-protect
        (let ((r (catch 'acp-cancel
                   (multiple-value-list
                    (eng:run prompt-text :history messages :system sys :verbose nil)))))
          (setf stop (if (eq r :cancelled) "cancelled" (finish-turn sess messages r))))
      (supdate sid :active nil)
      (supdate sid :worker nil)
      (send-response req-id (obj "stopReason" stop)))))

(defun start-prompt (req-id params)
  "Run a session/prompt turn here (the session actor, or inline with no router)."
  (let* ((sid (gethash "sessionId" params))
         (p (sget sid)))
    (if (null p)
        (send-error req-id -32602 (format nil "unknown sessionId ~A" sid))
        (handler-case (run-prompt-turn sid req-id (blocks->content (gethash "prompt" params)))
          (error (e)
            (format *log* "acp turn error: ~A~%" e)
            (ignore-errors (send-response req-id (obj "stopReason" "refusal"))))))))

;;; ------------------------ session/load replay -----------------------

(defun replay-history (sid sess)
  "Stream a loaded session's prior turns back to the client before responding."
  (loop for m in (session:session-history sess)
        for role = (gethash "role" m)
        for c = (gethash "content" m)
        when (and (stringp c) (plusp (length c)))
        do (cond
             ((string= role "user")
              (send-notification "session/update"
                (obj "sessionId" sid "update"
                     (obj "sessionUpdate" "user_message_chunk"
                          "messageId" (format nil "u_~D" (incf *msg-seq*))
                          "content" (text-block c)))))
             ((string= role "assistant")
              (send-notification "session/update"
                (obj "sessionId" sid "update"
                     (obj "sessionUpdate" "agent_message_chunk"
                          "messageId" (format nil "a_~D" (incf *msg-seq*))
                          "content" (text-block c))))))))

;;; ------------------------------ handlers ----------------------------

(defvar *client-caps* nil)

(defun h-initialize (params)
  (setf *client-caps* (and (hash-table-p params) (gethash "clientCapabilities" params)))
  (obj "protocolVersion" *protocol-version*
       "agentCapabilities" (obj "loadSession" t
                                "promptCapabilities" (obj "image" t "audio" nil "embeddedContext" t)
                                "mcpCapabilities" (obj "http" nil "sse" nil))
       "agentInfo" (obj "name" "operandi" "title" "operandi" "version" "0.1.0")
       "authMethods" #()))

(defun h-session-new (params)
  (let* ((sess (session:make-session))
         (sid (gethash :id sess))
         (cwd (and (hash-table-p params) (gethash "cwd" params))))
    (when cwd (ignore-errors (uiop:chdir cwd)))
    (sput sid (list :session sess :cwd cwd :allow nil :active nil :worker nil
                    ;; non-standard, honored: hosts like buzz-acp pass a system
                    ;; prompt here to configure the agent for their environment.
                    :system-prompt (and (hash-table-p params) (gethash "systemPrompt" params))))
    (obj "sessionId" sid)))

(defun h-session-load (params)
  (let* ((sid (gethash "sessionId" params))
         (cwd (gethash "cwd" params))
         (sess (session:make-session)))
    (if (session:resume-session! sess sid)
        (progn
          (sput sid (list :session sess :cwd cwd :allow nil :active nil :worker nil
                          :system-prompt (and (hash-table-p params) (gethash "systemPrompt" params))))
          (replay-history sid sess)
          (obj))
        (error "unknown sessionId ~A" sid))))

(defun handle-request (id method params)
  (handler-case
      (cond
        ((string= method "initialize")     (send-response id (h-initialize params)))
        ((string= method "authenticate")   (send-response id (obj)))
        ((and *router* (member method '("session/new" "session/load") :test #'string=))
         (open-session id method params))
        ((string= method "session/new")    (send-response id (h-session-new params)))
        ((string= method "session/load")   (send-response id (h-session-load params)))
        ((string= method "session/prompt")
         (if *router* (to-session id (gethash "sessionId" params) (list :prompt id params))
             (start-prompt id params)))
        (t (send-error id -32601 (format nil "method not found: ~A" method))))
    (error (e) (send-error id -32603 (princ-to-string e)))))

(defun handle-notification (method params)
  (cond
    ((and *router* (string= method "session/cancel"))
     (to-session nil (gethash "sessionId" params) :cancel))
    (t nil)))

(defun handle-message (msg)
  (let ((id (gethash "id" msg)) (method (gethash "method" msg)))
    (cond
      ((and method id) (handle-request id method (gethash "params" msg)))
      (method           (handle-notification method (gethash "params" msg)))
      (id               (route-response id msg)))))

;;; --------------------------- session actors -------------------------

(defun to-session (req-id sid msg)
  "Router: forward MSG to SID's actor, or answer REQ-ID that there is none."
  (let ((a (getf (sget sid) :actor)))
    (cond (a (actor:send a msg))
          (req-id (send-error req-id -32602 (format nil "unknown sessionId ~A" sid))))))

(defun open-session (req-id method params)
  "Router: session/new or session/load starts the session's own actor, which
   answers REQ-ID itself once the session exists."
  (let ((cwd (and (hash-table-p params) (gethash "cwd" params))))
    (when cwd (ignore-errors (uiop:chdir cwd))))
  (actor:spawn 'session-actor *router* req-id method params))

(defun session-actor (router req-id method params)
  "Own one session: create or resume it, then run its turns as they come."
  (declare (ignore router))
  (let ((*sessions* (make-hash-table :test 'equal))
        (*rpc-seq* 0) (*msg-seq* 0) (*tool-seq* 0) (*tool-id* nil) (*sid* nil)
        (store:*exec-via* (lambda (sql params) (to-router (list :store sql params))))
        (*standard-output* *log*)
        (sid nil))
    (handler-case
        (if (string= method "session/new")
            (let ((r (h-session-new params)))
              (setf sid (gethash "sessionId" r))
              (to-router (list :session sid (actor:self)))
              (send-response req-id r))
            (progn
              (setf sid (gethash "sessionId" params))
              (to-router (list :session sid (actor:self)))
              (send-response req-id (h-session-load params))))
      (error (e)
        (when sid (to-router (list :session sid nil)))
        (send-error req-id -32603 (princ-to-string e))
        (return-from session-actor nil)))
    (loop
      (let ((m (actor:receive)))
        (cond ((eq m :stop) (return))
              ((and (consp m) (eq (first m) :prompt)) (start-prompt (second m) (third m)))
              (t nil))))))                    ; a late :cancel or reply: nothing waits for it

;;; ------------------------------- router -----------------------------

(defun router-handle (msg)
  "Everything the router does, one message at a time."
  (case (first msg)
    (:out     (emit (second msg)))
    (:in      (let ((s (string-trim '(#\Space #\Tab #\Return #\Newline) (second msg))))
                (when (plusp (length s))
                  (let ((m (ignore-errors (json:parse s))))
                    (if (hash-table-p m)
                        (handler-case (handle-message m)
                          (error (e) (format *log* "acp: handler error: ~A~%" e)))
                        (format *log* "acp: unparseable line~%"))))))
    (:pending (setf (gethash (second msg) *pending*) (third msg)))
    (:session (if (third msg)
                  (sput (second msg) (list :actor (third msg)))
                  (remhash (second msg) *sessions*)))
    (:store   (handler-case (apply #'store:exec-non-query (second msg) (third msg))
                (error (e) (format *log* "acp: store write failed: ~A~%" e))))
    (t nil)))

(defun reader-loop (router source)
  "Send each line of SOURCE to ROUTER, then :EOF.  SOURCE is a stream (threads
   share it), :STDIN or a file name (an actor opens its own, since it may not
   use the router's stream objects)."
  (let ((in (if (streamp source)
                source
                (open (if (eq source :stdin) "/dev/stdin" source) :external-format :utf-8))))
    (loop for line = (read-line in nil :eof)
          until (eq line :eof)
          do (actor:send router (list :in line)))
    (actor:send router (list :eof))))

;;; ------------------------------- serve ------------------------------

(defun serve (&key (in *standard-input*) (out *standard-output*))
  "Run the ACP server over IN/OUT until EOF. Blocks. All incidental output is
   redirected to *error-output* so only JSON-RPC reaches OUT."
  (setf *out* out *log* *error-output*
        *sessions* (make-hash-table :test 'equal)
        *pending* (make-hash-table :test 'equal)
        *rpc-seq* 0 *msg-seq* 0 *tool-seq* 0 *outq* nil
        *router* (actor:self))
  (unwind-protect
      (let ((*standard-output* *log*))
        (actor:spawn 'reader-loop *router* #+modus (if (streamp in) :stdin in) #-modus in)
        (loop
          (let ((m (actor:receive)))
            (when (and (consp m) (eq (first m) :eof)) (return))
            (when (consp m) (router-handle m)))))
    (loop for p being the hash-values of *sessions*
          do (let ((a (getf p :actor))) (when a (ignore-errors (actor:send a :stop)))))
    (setf *router* nil)))
