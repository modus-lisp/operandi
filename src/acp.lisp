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
;;;   the concurrent-turn model (worker thread + main read loop + a pending-id
;;;     map) is the same one the TUI uses — needed because a turn may call the
;;;     client (permission) and block while staying responsive to session/cancel.
;;;
;;; Entry: (operandi.acp:serve).  Everything incidental (engine/tool prints,
;;; warnings) is routed to *error-output* so only JSON-RPC touches stdout.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (ql:quickload '(:com.inuoe.jzon :bordeaux-threads :uiop) :silent t))

(defpackage #:operandi.acp
  (:use #:cl)
  (:local-nicknames (#:eng     #:operandi.engine)
                    (#:llm     #:operandi.llm)
                    (#:tools   #:operandi.tools)
                    (#:hooks   #:operandi.hooks)
                    (#:session #:operandi.session)
                    (#:jzon    #:com.inuoe.jzon)
                    (#:bt      #:bordeaux-threads))
  (:export #:serve #:*protocol-version*))

(in-package #:operandi.acp)

(defparameter *protocol-version* 1)
(defparameter +null+ (find-symbol "NULL" "COM.INUOE.JZON") "The jzon value that serializes to JSON null.")

;;; ------------------------------ transport ---------------------------

(defvar *out* nil "Protocol output stream (real stdout). GLOBAL: worker threads write to it.")
(defvar *log* nil "Where incidental output goes (stderr).")
;; All output goes through a single writer thread draining a queue. Producers
;; (the streaming worker, the main loop, the permission gate) ENQUEUE and never
;; touch the stream — so a slow client filling the stdout pipe can only block the
;; writer, never a lock-holding producer. Writing under a shared lock instead
;; deadlocked: the streaming worker blocked mid-write holding the lock, so the
;; permission request could never be sent and the turn hung.
(defvar *outq* nil "FIFO of pending output lines (strings).")
(defvar *outq-lock* (bt:make-lock "acp-outq"))
(defvar *outq-cv* (bt:make-condition-variable :name "acp-outq"))
(defvar *writer* nil)
(defvar *writer-run* nil)
(defvar *rpc-seq* 0)
(defvar *rpc-seq-lock* (bt:make-lock "acp-rpc-seq"))
(defvar *pending* (make-hash-table) "Outbound request id -> reply box.")
(defvar *pending-lock* (bt:make-lock "acp-pending"))

(defun obj (&rest pairs)
  "String-keyed hash-table for jzon, OMITTING any key whose value is :SKIP."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          unless (eq v :skip) do (setf (gethash k h) v))
    h))

(defun send-line (message)
  "Enqueue MESSAGE for the writer thread. Never blocks on the stream."
  (let ((s (jzon:stringify message)))
    (bt:with-lock-held (*outq-lock*)
      (setf *outq* (nconc *outq* (list s)))
      (bt:condition-notify *outq-cv*))))

(defun writer-loop ()
  "The ONE thread that writes to *out*. Blocks only itself on a full pipe."
  (loop
    (let ((item (bt:with-lock-held (*outq-lock*)
                  (loop until (or (not *writer-run*) *outq*)
                        do (bt:condition-wait *outq-cv* *outq-lock*))
                  (if *outq* (pop *outq*) :stop))))
      (when (eq item :stop) (return))
      (when (stringp item)
        (write-string item *out*) (write-char #\Newline *out*) (force-output *out*)))))

(defun send-notification (method params)
  (send-line (obj "jsonrpc" "2.0" "method" method "params" params)))
(defun send-response (id result)
  (send-line (obj "jsonrpc" "2.0" "id" id "result" result)))
(defun send-error (id code msg)
  (send-line (obj "jsonrpc" "2.0" "id" id "error" (obj "code" code "message" msg))))

(defun next-rpc-id ()
  (bt:with-lock-held (*rpc-seq-lock*) (incf *rpc-seq*)))

;; Outbound request (agent -> client), blocking until the client replies. The
;; main read loop routes the reply back via *pending*.
(defun make-box () (list (bt:make-lock) (bt:make-condition-variable) (cons nil nil)))
(defun call-client (method params)
  (let ((id (next-rpc-id)) (box (make-box)))
    (bt:with-lock-held (*pending-lock*) (setf (gethash id *pending*) box))
    (send-line (obj "jsonrpc" "2.0" "id" id "method" method "params" params))
    (destructuring-bind (lock cv cell) box
      (bt:with-lock-held (lock)
        (loop until (car cell) do (bt:condition-wait cv lock)))
      (cdr cell))))
(defun route-response (id msg)
  (let ((box (bt:with-lock-held (*pending-lock*)
               (prog1 (gethash id *pending*) (remhash id *pending*)))))
    (when box
      (destructuring-bind (lock cv cell) box
        (bt:with-lock-held (lock)
          (setf (car cell) t (cdr cell) (gethash "result" msg))
          (bt:condition-notify cv))))))

;;; ------------------------------ sessions ----------------------------

(defvar *sessions* (make-hash-table :test 'equal) "sessionId -> plist (:session :worker :active :allow).")
(defvar *sessions-lock* (bt:make-lock "acp-sessions"))
(defun sget (sid) (bt:with-lock-held (*sessions-lock*) (gethash sid *sessions*)))
(defun sput (sid plist) (bt:with-lock-held (*sessions-lock*) (setf (gethash sid *sessions*) plist)))
(defun supdate (sid key val)
  (bt:with-lock-held (*sessions-lock*)
    (let ((p (gethash sid *sessions*))) (when p (setf (getf p key) val (gethash sid *sessions*) p)))))

;;; ------------------------- content <-> wire -------------------------

(defun text-block (s) (obj "type" "text" "text" (or s "")))

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
                   ((equal type "image") (format o "[image omitted]"))
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
  (let ((a (or (%arg args "file_path") (%arg args "command") (%arg args "pattern")
               (%arg args "url") (%arg args "query") (%arg args "form"))))
    (if (stringp a)
        (format nil "~A: ~A" name (if (> (length a) 60) (concatenate 'string (subseq a 0 60) "…") a))
        name)))
(defun tool-locations (name args)
  (let ((p (%arg args "file_path")))
    (if (and p (stringp p)) (vector (obj "path" p)) :skip)))

(defun clip (s n)
  (let ((s (if (stringp s) s (princ-to-string (or s "")))))
    (if (> (length s) n) (concatenate 'string (subseq s 0 n) "…") s)))

(defun tool-failed-p (result)
  (and (stringp result)
       (or (eql 0 (search "TOOL ERROR" result)) (eql 0 (search "REFUSED" result)))))

;;; --------------------- per-turn streaming hooks ---------------------

(defvar *sid* nil "Current sessionId during a prompt turn (bound on the worker).")
(defvar *msg-id* nil)
(defvar *msg-seq* 0)
(defvar *tool-ids* (make-hash-table) "worker-thread -> current toolCallId (pre/post correlation).")
(defvar *tool-ids-lock* (bt:make-lock "acp-tool-ids"))
(defvar *tool-seq* 0)

(defun update! (u) (when *sid* (send-notification "session/update" (obj "sessionId" *sid* "update" u))))

(defun acp-on-token (tok)
  (when (and tok (plusp (length tok)))
    (update! (obj "sessionUpdate" "agent_message_chunk"
                  "messageId" *msg-id* "content" (text-block tok)))))

(defun acp-pre-hook (name args)
  (when *sid*
    (let ((id (bt:with-lock-held (*tool-ids-lock*)
                (setf (gethash (bt:current-thread) *tool-ids*)
                      (format nil "call_~D" (incf *tool-seq*))))))
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
    (let ((id (bt:with-lock-held (*tool-ids-lock*)
                (prog1 (gethash (bt:current-thread) *tool-ids*)
                  (remhash (bt:current-thread) *tool-ids*)))))
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
         (outcome (and (hash-table-p resp) (gethash "outcome" resp))))
    (cond
      ((equal outcome "selected")
       (let ((opt (gethash "optionId" resp)))
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
              (let ((tool-id (bt:with-lock-held (*tool-ids-lock*)
                               (gethash (bt:current-thread) *tool-ids*))))
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
         (*standard-output* *log*)                          ; keep the protocol channel clean
         (hooks:*pre-tool-hooks*  (cons #'acp-pre-hook hooks:*pre-tool-hooks*))
         (hooks:*post-tool-hooks* (cons #'acp-post-hook hooks:*post-tool-hooks*))
         (tools:*tool-gate* (lambda (name args) (acp-gate sid name args)))
         (eng:*stream* t)
         (eng:*on-token* #'acp-on-token)
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
  (let* ((sid (gethash "sessionId" params))
         (p (sget sid)))
    (cond
      ((null p) (send-error req-id -32602 (format nil "unknown sessionId ~A" sid)))
      (t
       (let* ((cwd (getf p :cwd))
              (text (blocks->text (gethash "prompt" params))))
         (when cwd (ignore-errors (uiop:chdir cwd)))
         (let ((w (bt:make-thread
                   (lambda ()
                     (handler-case (run-prompt-turn sid req-id text)
                       (error (e)
                         (format *log* "acp turn error: ~A~%" e)
                         (ignore-errors (send-response req-id (obj "stopReason" "refusal"))))))
                   :name (format nil "acp-turn-~A" sid))))
           (supdate sid :worker w)))))))

(defun cancel-session (sid)
  (let ((p (sget sid)))
    (when (and p (getf p :active) (getf p :worker) (bt:thread-alive-p (getf p :worker)))
      (ignore-errors
       (bt:interrupt-thread (getf p :worker) (lambda () (throw 'acp-cancel :cancelled)))))))

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
                                "promptCapabilities" (obj "image" nil "audio" nil "embeddedContext" t)
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
        ((string= method "session/new")    (send-response id (h-session-new params)))
        ((string= method "session/load")   (send-response id (h-session-load params)))
        ((string= method "session/prompt") (start-prompt id params))   ; responds async
        (t (send-error id -32601 (format nil "method not found: ~A" method))))
    (error (e) (send-error id -32603 (princ-to-string e)))))

(defun handle-notification (method params)
  (cond
    ((string= method "session/cancel") (cancel-session (gethash "sessionId" params)))
    (t nil)))

(defun handle-message (msg)
  (let ((id (gethash "id" msg)) (method (gethash "method" msg)))
    (cond
      ((and method id) (handle-request id method (gethash "params" msg)))
      (method           (handle-notification method (gethash "params" msg)))
      (id               (route-response id msg)))))

;;; ------------------------------- serve ------------------------------

(defun serve (&key (in *standard-input*) (out *standard-output*))
  "Run the ACP server over IN/OUT until EOF. Blocks. All incidental output is
   redirected to *error-output* so only JSON-RPC reaches OUT."
  (setf *out* out *log* *error-output*
        *sessions* (make-hash-table :test 'equal)
        *pending* (make-hash-table)
        *rpc-seq* 0 *msg-seq* 0 *tool-seq* 0
        *tool-ids* (make-hash-table)
        *outq* nil *writer-run* t)
  (setf *writer* (bt:make-thread #'writer-loop :name "acp-writer"))
  (unwind-protect
      (let ((*standard-output* *log*))
        (loop
          (let ((line (read-line in nil :eof)))
            (when (eq line :eof) (return))
            (let ((s (string-trim '(#\Space #\Tab #\Return #\Newline) line)))
              (when (plusp (length s))
                (let ((msg (ignore-errors (jzon:parse s))))
                  (if (hash-table-p msg)
                      (handler-case (handle-message msg)
                        (error (e) (format *log* "acp: handler error: ~A~%" e)))
                      (format *log* "acp: unparseable line~%"))))))))
    ;; drain + stop the writer
    (bt:with-lock-held (*outq-lock*) (setf *writer-run* nil) (bt:condition-notify *outq-cv*))
    (ignore-errors (bt:join-thread *writer*))))
