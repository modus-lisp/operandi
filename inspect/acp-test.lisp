;;; inspect/acp-test.lisp
;;;
;;; Oracle for the Agent Client Protocol server (operandi.acp). Deterministic:
;;; no live model, no transport threads. It captures what the server would write
;;; to the wire (by binding acp::*out* to a string) and drives the handlers +
;;; ONE stubbed prompt turn, asserting the exact session/update stream and the
;;; JSON-RPC response shapes against the ACP spec (protocolVersion 1, the
;;; agent_message_chunk / tool_call / tool_call_update / usage_update updates,
;;; stopReason, tool kinds, content-block extraction, and the permission gate).
;;;
;;; Exit 0 iff all checks pass; joins fitness as a swarm/fitness oracle.

(require :asdf)
(funcall (read-from-string "ql:quickload") :operandi :silent t)
(funcall (find-symbol "OPEN-STORE" "OPERANDI.STORE"))

(defpackage #:acp-test (:use #:cl)
  (:local-nicknames (#:acp #:operandi.acp) (#:llm #:operandi.llm)
                    (#:eng #:operandi.engine) (#:session #:operandi.session)
                    (#:jzon #:com.inuoe.jzon)))
(in-package #:acp-test)

(defvar *fails* 0)
(defmacro check (name form)
  `(handler-case (if ,form (format t "  ok   ~A~%" ,name)
                     (progn (incf *fails*) (format t "  FAIL ~A~%" ,name)))
     (error (e) (incf *fails*) (format t "  ERR  ~A: ~A~%" ,name e))))

;; isolate persistence + capture the protocol channel
(setf (symbol-value (find-symbol "*SESSIONS-DIR*" "OPERANDI.SESSION"))
      (namestring (merge-pathnames "acp-test-sessions/" (uiop:temporary-directory))))
(setf acp::*out* (make-string-output-stream)
      acp::*log* (make-broadcast-stream)
      acp::*sessions* (make-hash-table :test 'equal)
      acp::*pending* (make-hash-table)
      acp::*rpc-seq* 0 acp::*msg-seq* 0 acp::*tool-seq* 0
      acp::*tool-ids* (make-hash-table))

(defun drain ()
  "Parse everything written to the wire since the last drain."
  (let ((s (get-output-stream-string acp::*out*)))
    (loop for line in (uiop:split-string s :separator '(#\Newline))
          when (plusp (length (string-trim " " line)))
          collect (jzon:parse line))))
(defun m@ (m &rest path)
  (dolist (k path m)
    (setf m (cond ((and (integerp k) (vectorp m) (< k (length m))) (aref m k))
                  ((and (integerp k) (listp m) (< k (length m))) (nth k m))
                  ((hash-table-p m) (gethash k m))
                  (t nil)))))
(defun updates (msgs kind)
  (loop for m in msgs
        when (and (equal (m@ m "method") "session/update")
                  (equal (m@ m "params" "update" "sessionUpdate") kind))
        collect (m@ m "params" "update")))

(format t "~&== initialize ==~%")
(acp::handle-message (llm:ht "jsonrpc" "2.0" "id" 0 "method" "initialize"
                             "params" (llm:ht "protocolVersion" 1
                                              "clientCapabilities" (llm:ht "fs" (llm:ht "readTextFile" t "writeTextFile" t)))))
(let ((r (m@ (first (drain)) "result")))
  (check "protocolVersion is 1"     (eql 1 (m@ r "protocolVersion")))
  (check "advertises loadSession"   (eq t (m@ r "agentCapabilities" "loadSession")))
  (check "advertises embeddedContext" (eq t (m@ r "agentCapabilities" "promptCapabilities" "embeddedContext")))
  (check "agentInfo name"           (equal "operandi" (m@ r "agentInfo" "name"))))

(format t "~&== session/new ==~%")
(acp::handle-message (llm:ht "jsonrpc" "2.0" "id" 1 "method" "session/new"
                             "params" (llm:ht "cwd" (namestring (uiop:temporary-directory)) "mcpServers" #())))
(defparameter *sid* (m@ (first (drain)) "result" "sessionId"))
(check "session/new returns a sessionId" (stringp *sid*))
(check "session registered"              (acp::sget *sid*))

(format t "~&== content-block extraction + tool kinds ==~%")
(check "text blocks concatenate"
       (search "hello world"
               (acp::blocks->text (vector (llm:ht "type" "text" "text" "hello ")
                                          (llm:ht "type" "text" "text" "world")))))
(check "resource block embeds text"
       (search "def foo"
               (acp::blocks->text (vector (llm:ht "type" "resource"
                                                  "resource" (llm:ht "uri" "file:///x.py" "text" "def foo(): pass"))))))
(check "Read -> read kind"    (equal "read" (acp::tool-kind "Read")))
(check "Bash -> execute kind" (equal "execute" (acp::tool-kind "Bash")))
(check "Edit -> edit kind"    (equal "edit" (acp::tool-kind "Edit")))

(format t "~&== a stubbed prompt turn streams the right updates ==~%")
(let ((orig (fdefinition (find-symbol "RUN" "OPERANDI.ENGINE"))))
  (setf (fdefinition (find-symbol "RUN" "OPERANDI.ENGINE"))
        (lambda (prompt &key history verbose &allow-other-keys)
          (declare (ignore prompt verbose))
          ;; stream two assistant chunks
          (funcall eng:*on-token* "Hello ")
          (funcall eng:*on-token* "world")
          ;; one tool call through the hook chain (as invoke-tool would)
          (operandi.hooks:run-pre-hooks "Read" (llm:ht "file_path" "/proj/x.lisp"))
          (operandi.hooks:run-post-hooks "Read" (llm:ht "file_path" "/proj/x.lisp") "the file body" nil 5)
          (values "Hello world"
                  (append history (list (llm:ht "role" "assistant" "content" "Hello world")))
                  1 (llm:make-usage :prompt-tokens 10 :completion-tokens 2 :cost-usd 0.0001d0))))
  (unwind-protect
      (progn
        (acp::run-prompt-turn *sid* 42 "hi there")
        (let* ((msgs (drain))
               (chunks (updates msgs "agent_message_chunk"))
               (tcalls (updates msgs "tool_call"))
               (tupd   (updates msgs "tool_call_update"))
               (usage  (updates msgs "usage_update"))
               (resp   (find-if (lambda (m) (eql 42 (m@ m "id"))) msgs)))
          (check "streamed agent_message_chunks"
                 (equal "Hello world"
                        (apply #'concatenate 'string
                               (mapcar (lambda (u) (m@ u "content" "text")) chunks))))
          (check "tool_call announced with kind read"
                 (and tcalls (equal "read" (m@ (first tcalls) "kind"))))
          (check "tool_call has a locations path"
                 (equal "/proj/x.lisp" (m@ (first tcalls) "locations" 0 "path")))
          (check "tool_call_update completed"
                 (and tupd (equal "completed" (m@ (first tupd) "status"))))
          (check "tool_call ids match pre/post"
                 (equal (m@ (first tcalls) "toolCallId") (m@ (first tupd) "toolCallId")))
          (check "usage_update sent"       (and usage (m@ (first usage) "used")))
          (check "response stopReason end_turn"
                 (equal "end_turn" (m@ resp "result" "stopReason")))
          (check "turn persisted to the session"
                 (= 1 (gethash :turns (getf (acp::sget *sid*) :session))))))
    (setf (fdefinition (find-symbol "RUN" "OPERANDI.ENGINE")) orig)))

(format t "~&== permission gate ==~%")
(let ((orig (and (fboundp (find-symbol "CALL-CLIENT" "OPERANDI.ACP"))
                 (fdefinition (find-symbol "CALL-CLIENT" "OPERANDI.ACP"))))
      (decision "reject"))
  (setf (fdefinition (find-symbol "CALL-CLIENT" "OPERANDI.ACP"))
        (lambda (method params)
          (declare (ignore method params))
          (if (equal decision "reject")
              (llm:ht "outcome" "cancelled")
              (llm:ht "outcome" "selected" "optionId" decision))))
  (unwind-protect
      (progn
        (check "read tool auto-allowed (no client call)"
               (null (acp::acp-gate *sid* "Read" (llm:ht "file_path" "/x"))))
        (setf decision "reject")
        (check "execute tool denied on reject -> REFUSED string"
               (let ((r (acp::acp-gate *sid* "Bash" (llm:ht "command" "rm -rf /"))))
                 (and (stringp r) (search "denied" r))))
        (setf decision "always")
        (check "execute tool allowed on 'always' -> NIL"
               (null (acp::acp-gate *sid* "Bash" (llm:ht "command" "make"))))
        (check "'always' is remembered for the session"
               (null (acp::acp-gate *sid* "Bash" (llm:ht "command" "make test")))))
    (when orig (setf (fdefinition (find-symbol "CALL-CLIENT" "OPERANDI.ACP")) orig))))

(format t "~&== host systemPrompt is honored (Buzz-compat) ==~%")
(let ((captured nil)
      (orig (fdefinition (find-symbol "RUN" "OPERANDI.ENGINE"))))
  (setf (fdefinition (find-symbol "RUN" "OPERANDI.ENGINE"))
        (lambda (prompt &key history system verbose &allow-other-keys)
          (declare (ignore prompt verbose))
          (setf captured system)
          (values "ok" (append history (list (llm:ht "role" "assistant" "content" "ok")))
                  1 (llm:make-usage))))
  (unwind-protect
      (progn
        (acp::handle-message
         (llm:ht "jsonrpc" "2.0" "id" 20 "method" "session/new"
                 "params" (llm:ht "cwd" (namestring (uiop:temporary-directory)) "mcpServers" #()
                                  "systemPrompt" "BUZZ-ENV: reply via the buzz CLI.")))
        (let ((sid2 (m@ (first (drain)) "result" "sessionId")))
          (check "systemPrompt stored on the session"
                 (equal "BUZZ-ENV: reply via the buzz CLI." (getf (acp::sget sid2) :system-prompt)))
          (acp::run-prompt-turn sid2 21 "hi")
          (drain)
          (check "host prompt reaches eng:run :system"
                 (and (stringp captured) (search "BUZZ-ENV" captured)))
          (check "combined WITH operandi's own base prompt"
                 (and (stringp captured) (search "operandi" captured)))))
    (setf (fdefinition (find-symbol "RUN" "OPERANDI.ENGINE")) orig)))

(format t "~&== session/load replays history ==~%")
(let ((s2 (session:make-session)))
  (setf (gethash :history s2) (list (llm:ht "role" "user" "content" "earlier question")
                                    (llm:ht "role" "assistant" "content" "earlier answer")))
  (session:persist-session s2)
  (drain)
  (acp::handle-message (llm:ht "jsonrpc" "2.0" "id" 7 "method" "session/load"
                               "params" (llm:ht "sessionId" (gethash :id s2)
                                                "cwd" (namestring (uiop:temporary-directory)) "mcpServers" #())))
  (let* ((msgs (drain))
         (users (updates msgs "user_message_chunk"))
         (agents (updates msgs "agent_message_chunk"))
         (resp (find-if (lambda (m) (eql 7 (m@ m "id"))) msgs)))
    (check "load replays the user message"      (and users (search "earlier question" (m@ (first users) "content" "text"))))
    (check "load replays the agent message"     (and agents (search "earlier answer" (m@ (first agents) "content" "text"))))
    (check "load responds (result present)"     (and resp (nth-value 1 (gethash "result" resp))))))

(format t "~&~%acp-test: ~A failure~:P~%" *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
