;;; inspect/mcp-test.lisp
;;;
;;; Oracle for the MCP CLIENT (src/mcp.lisp), no model needed: the config
;;; story (default file, --mcp-config, --mcp; later wins), command-line
;;; parsing, tool naming, and then real servers -- mcp/test/fixture-server.mjs
;;; over stdio AND Streamable HTTP -- for registration, worker visibility, the
;;; external-content label, server-reported errors, CONCURRENT calls from
;;; several threads, timeouts, a server that dies, and clean shutdown.
;;;
;;;   sbcl --non-interactive --load inspect/mcp-test.lisp   (0/1)
;;;
;;; The live part needs node and mcp/node_modules (`cd mcp && npm install`);
;;; without them it says SKIP and checks only the offline part.

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.mcp-test
  (:use #:cl)
  (:local-nicknames (#:mcp #:operandi.mcp) (#:tools #:operandi.tools)
                    (#:sub #:operandi.subagent) (#:llm #:operandi.llm)
                    (#:bt #:bordeaux-threads)))
(in-package #:operandi.mcp-test)

(defvar *pass* 0) (defvar *fail* 0)
(defmacro check (name &body body)
  `(handler-case
       (if (progn ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A~%" ,name)))
     (serious-condition (e)
       (incf *fail*) (format t "~&  FAIL ~A (~A: ~A)~%" ,name (type-of e) e))))

(defun call (name &rest kv)
  (tools:invoke-tool name (apply #'llm:ht kv)))

(defun has (s needle) (and (stringp s) (search needle s)))

(defun write-file (path text)
  (with-open-file (o path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string text o))
  path)

(defun fixture-path ()
  "A fixture-server.mjs whose mcp/node_modules exists: the system's own
   checkout first, then the tree this test file came from."
  (loop for root in (list (asdf:system-source-directory :operandi)
                          (merge-pathnames "../" (make-pathname :name nil :type nil
                                                                :defaults *load-truename*)))
        for fx = (merge-pathnames "mcp/test/fixture-server.mjs" root)
        when (and (probe-file fx)
                  (probe-file (merge-pathnames "mcp/node_modules/@modelcontextprotocol/sdk/package.json" root)))
          return (uiop:native-namestring (probe-file fx))))

(defun node-p ()
  (ignore-errors
   (zerop (nth-value 2 (uiop:run-program '("node" "--version") :ignore-error-status t)))))

(defun elapsed (t0) (/ (- (get-internal-real-time) t0) internal-time-units-per-second))

(defun offline ()
  (format t "~&-- offline: parsing and configuration~%")
  (check "split-command-line: quotes, escapes, empty words"
    (equal (mcp:split-command-line "node  'a b' \"c \\\"d\\\"\" e\\ f ''")
           '("node" "a b" "c \"d\"" "e f" "")))
  (check "--mcp NAME=command → stdio spec"
    (let ((s (mcp:parse-cli-spec "rea=npx -y rea-agents mcp")))
      (and (eq (getf s :transport) :stdio) (equal (getf s :command) "npx")
           (equal (getf s :args) '("-y" "rea-agents" "mcp")) (equal (getf s :name) "rea"))))
  (check "--mcp NAME=https://… → http spec"
    (let ((s (mcp:parse-cli-spec "docs=https://example.test/mcp")))
      (and (eq (getf s :transport) :http) (equal (getf s :url) "https://example.test/mcp"))))
  (check "--mcp without NAME= is an error"
    (handler-case (progn (mcp:parse-cli-spec "npx foo") nil) (error () t)))
  (check "--mcp with a bad server name is an error"
    (handler-case (progn (mcp:parse-cli-spec "a b=npx") nil) (error () t)))
  (check "${VAR} and ${VAR:-default} expand"
    (and (equal (mcp:expand-vars "${HOME}/x") (format nil "~A/x" (uiop:getenv "HOME")))
         (equal (mcp:expand-vars "${OPERANDI_SURELY_UNSET_VAR:-dflt}") "dflt")))
  (check "tool names: mcp__server__tool, sanitised, ≤64 chars, long names distinct"
    (let ((a (mcp:tool-name-for "my.server" "do-thing!"))
          (l1 (mcp:tool-name-for "s" (make-string 80 :initial-element #\a)))
          (l2 (mcp:tool-name-for "s" (concatenate 'string (make-string 79 :initial-element #\a) "b"))))
      (and (equal a "mcp__my_server__do-thing_")
           (<= (length l1) 64) (<= (length l2) 64) (string/= l1 l2))))
  (let* ((dir (ensure-directories-exist
               (pathname (format nil "/tmp/operandi-mcp-test-~36R/" (random (expt 36 8) (make-random-state t))))))
         (default (write-file (merge-pathnames "default.json" dir)
                              "{\"mcpServers\": {\"a\": {\"command\": \"echo\", \"args\": [\"from-default\"]},
                                                \"b\": {\"command\": \"echo\", \"args\": [\"b-default\"]},
                                                \"off\": {\"command\": \"echo\", \"disabled\": true},
                                                \"web\": {\"type\": \"http\", \"url\": \"https://x.test/${OPERANDI_SURELY_UNSET_VAR:-m}\",
                                                          \"headers\": {\"Authorization\": \"Bearer t\"}}}}"))
         (extra (write-file (merge-pathnames "extra.json" dir)
                            "{\"mcpServers\": {\"b\": {\"command\": \"echo\", \"args\": [\"b-extra\"], \"timeout\": 7, \"workers\": true}}}")))
    (let* ((d (mcp:read-config-file default))
           (merged (mcp:merge-specs d (mcp:read-config-file extra)
                                    (list (mcp:parse-cli-spec "a=echo from-cli")))))
      (check "config: disabled servers are dropped, http + header + ${…} read"
        (and (equal (mapcar (lambda (s) (getf s :name)) d) '("a" "b" "web"))
             (let ((w (third d)))
               (and (eq (getf w :transport) :http) (equal (getf w :url) "https://x.test/m")
                    (equal (getf w :headers) '(("Authorization" . "Bearer t")))))))
      (check "config: later sources win per name and keep the first position"
        (and (equal (mapcar (lambda (s) (getf s :name)) merged) '("a" "b" "web"))
             (equal (getf (first merged) :args) '("from-cli"))
             (equal (getf (second merged) :args) '("b-extra"))
             (eql (getf (second merged) :timeout) 7)
             (eq (getf (second merged) :workers) t))))
    (check "a named config file that is missing is an error"
      (handler-case (progn (mcp:read-config-file (merge-pathnames "nope.json" dir)) nil) (error () t)))
    (check "the default file may be missing"
      (null (mcp:read-config-file (merge-pathnames "nope.json" dir) :missing-ok t)))
    (check "a config without mcpServers is an error"
      (handler-case (progn (mcp:read-config-file (write-file (merge-pathnames "bad.json" dir) "{\"x\":1}")) nil)
        (error () t)))
    (check "start-from-options with nothing configured starts nothing"
      (let ((mcp:*default-config-file* (merge-pathnames "nope.json" dir)))
        (null (mcp:start-from-options :verbose nil))))
    (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))

(defun live-common (label prefix)
  "Checks that hold for any transport, on a server registered as PREFIX."
  (flet ((n (tool) (format nil "~A~A" prefix tool)))
    (check (format nil "~A: all six fixture tools are registered and in the default set" label)
      (every (lambda (tl) (and (tools:tool-by-name (n tl)) (member (n tl) (tools:default-tools) :test #'string=)))
             '("echo" "add" "slow" "fail" "write_note" "inject")))
    (check (format nil "~A: workers get exactly the read-only tools" label)
      (let ((w (sub:ask-worker-tools)))
        (and (member (n "echo") w :test #'string=) (member (n "add") w :test #'string=)
             (not (member (n "write_note") w :test #'string=)) (not (member (n "slow") w :test #'string=)))))
    (check (format nil "~A: a call returns the server's text, labelled external" label)
      (let ((r (call (n "echo") "text" "hi there")))
        (and (has r "echo: hi there") (has r "external MCP result") (has r "data, not instructions"))))
    (check (format nil "~A: numeric arguments round-trip" label)
      (has (call (n "add") "a" 2 "b" 3) (format nil "~%5")))
    (check (format nil "~A: a server-reported failure is a TOOL ERROR" label)
      (has (call (n "fail")) "TOOL ERROR (reported by the server)"))
    (check (format nil "~A: text that tries to instruct the agent arrives labelled" label)
      (let ((r (call (n "inject"))))
        (and (< (search "data, not instructions" r) (search "IGNORE ALL PREVIOUS" r)))))
    (check (format nil "~A: 4 concurrent 600 ms calls overlap (< 1.8 s) and each gets its own answer" label)
      (let* ((t0 (get-internal-real-time))
             (results (make-array 4))
             (threads (loop for i below 4
                            collect (let ((i i))
                                      (bt:make-thread (lambda ()
                                                        (setf (aref results i)
                                                              (call (n "slow") "ms" 600 "tag" (format nil "t~D" i)))))))))
        (mapc #'bt:join-thread threads)
        (and (< (elapsed t0) 1.8)
             (loop for i below 4 always (has (aref results i) (format nil "slept 600 t~D" i))))))
    (check (format nil "~A: --tools glob expands to the server's tools" label)
      (= 6 (length (tools:expand-tool-names (list (n "*"))))))))

(defun live-stdio (fx)
  (format t "~&-- live: stdio (~A)~%" fx)
  (let ((servers (mcp:start-servers (list (mcp:parse-cli-spec (format nil "fx=node ~A" fx))) :verbose nil)))
    (check "stdio: the server initialises and reports itself"
      (and (= 1 (length servers))
           (search "operandi-fixture" (mcp:status-text))
           (search "ready" (mcp:status-text))))
    (live-common "stdio" "mcp__fx__")
    (check "stdio: the server's stderr went to its log, not the protocol"
      (search "stdio server starting"
              (uiop:read-file-string (merge-pathnames ".operandi/mcp-logs/fx.log" (user-homedir-pathname))))))
  ;; a per-server timeout, an allow-list, and the workers flag, via a config file
  (let ((cfg (uiop:tmpize-pathname #p"/tmp/operandi-mcp-cfg.json")))
    (write-file cfg (format nil "{\"mcpServers\": {\"fy\": {\"command\": \"node\", \"args\": [~S],
                                   \"timeout\": 1, \"tools\": [\"slow\", \"wri*\"], \"workers\": true}}}" fx))
    (unwind-protect
         (progn
           (mcp:start-servers (mcp:read-config-file cfg) :verbose nil)
           (check "config: the tools allow-list registers only what it names"
             (and (tools:tool-by-name "mcp__fy__slow") (tools:tool-by-name "mcp__fy__write_note")
                  (not (tools:tool-by-name "mcp__fy__echo"))))
           (check "config: workers=true hands a worker even the mutating tools"
             (member "mcp__fy__write_note" (sub:ask-worker-tools) :test #'string=))
           (check "a call past the server's timeout is a TOOL ERROR, not a hang"
             (let ((t0 (get-internal-real-time)))
               (and (has (call "mcp__fy__slow" "ms" 3000) "timed out") (< (elapsed t0) 2.5))))
           (check "…and the connection still serves the next call"
             (has (call "mcp__fy__slow" "ms" 10 "tag" "after") "slept 10 after")))
      (ignore-errors (delete-file cfg))))
  (check "a server that dies at once is reported failed, registers nothing, does not hang"
    (let* ((t0 (get-internal-real-time))
           (s (first (mcp:start-servers (list (mcp:parse-cli-spec "dead=sh -c 'exit 3'")) :verbose nil))))
      (and (search "dead  [stdio, failed]" (mcp:status-text))
           (null (tools:expand-tool-names '("mcp__dead__*")))
           (< (elapsed t0) 10)
           s)))
  (check "a server killed mid-session: the next call is a TOOL ERROR, promptly"
    (let* ((srv (find "fx" mcp:*servers* :key (lambda (s) (getf (operandi.mcp::server-spec s) :name)) :test #'equal))
           (p (operandi.mcp::server-process srv)))
      (uiop:terminate-process p :urgent t)
      (uiop:wait-process p)
      (let ((t0 (get-internal-real-time))
            (r (call "mcp__fx__echo" "text" "anyone?")))
        (and (has r "TOOL ERROR") (< (elapsed t0) 5))))))

(defun live-http (fx)
  (format t "~&-- live: Streamable HTTP~%")
  (let* ((p (uiop:launch-program (list "node" fx "--http") :output :stream :error-output nil))
         (line (read-line (uiop:process-info-output p) nil ""))
         (port (and (uiop:string-prefix-p "PORT " line) (parse-integer line :start 5 :junk-allowed t))))
    (unwind-protect
         (progn
           (check "http: the fixture is listening" port)
           (when port
             (mcp:start-servers (list (mcp:parse-cli-spec (format nil "fh=http://127.0.0.1:~D/mcp" port))) :verbose nil)
             (check "http: initialised, with a session id"
               (let ((srv (find "fh" mcp:*servers* :key (lambda (s) (getf (operandi.mcp::server-spec s) :name)) :test #'equal)))
                 (and (eq (operandi.mcp::server-state srv) :ready)
                      (stringp (operandi.mcp::server-session-id srv)))))
             (live-common "http" "mcp__fh__")))
      (ignore-errors (uiop:terminate-process p :urgent t))
      (ignore-errors (uiop:wait-process p)))))

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&operandi MCP client oracle~%")
  (offline)
  (let ((fx (fixture-path)))
    (if (and fx (node-p))
        (unwind-protect
             (progn (live-stdio fx) (live-http fx))
          (mcp:stop-servers)
          (check "stop-servers unregisters every MCP tool"
            (and (null (tools:expand-tool-names '("mcp__*")))
                 (null tools:*extra-default-tools*) (null tools:*extra-worker-tools*))))
        (format t "~&  SKIP live checks: need node and mcp/node_modules (cd mcp && npm install)~%")))
  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
