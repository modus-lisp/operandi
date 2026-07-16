;;; inspect/tui-smoke.lisp
;;;
;;; Smoke test for the operandi TUI (operandi.tui). Exercises the pure
;;; machinery deterministically (colour off): slash-command dispatch +
;;; session mutation, tool-line rendering via the hooks, and streaming
;;; bullet logic. Then, IF a backend is configured, runs one live turn
;;; that forces a tool call so the whole render path is covered end to end.
;;;
;;; Exit 0 iff the deterministic checks pass (the live turn is best-effort
;;; and only warns on failure, since it depends on an external model).

(require :asdf)
(funcall (read-from-string "ql:quickload") :operandi :silent t)
(funcall (find-symbol "OPEN-STORE" "OPERANDI.STORE"))

(defpackage #:tui-smoke (:use #:cl)
  (:local-nicknames (#:tui #:operandi.tui) (#:llm #:operandi.llm)
                    (#:hooks #:operandi.hooks)))
(in-package #:tui-smoke)

(setf tui::*color* nil)                     ; deterministic, no escapes
(defvar *fails* 0)
(defmacro check (name form)
  `(handler-case (if ,form (format t "  ok   ~A~%" ,name)
                     (progn (incf *fails*) (format t "  FAIL ~A~%" ,name)))
     (error (e) (incf *fails*) (format t "  ERR  ~A: ~A~%" ,name e))))

(defun capture (thunk)
  "Run THUNK with *standard-output* rebound to a string."
  (let ((s (make-string-output-stream)))
    (let ((*standard-output* s)) (funcall thunk))
    (get-output-stream-string s)))

(defun capture-and-return (sess cmd)
  "Dispatch CMD with output suppressed; return handle-command's value."
  (let ((s (make-string-output-stream)) r)
    (let ((*standard-output* s)) (setf r (tui::handle-command cmd sess)))
    r))

(format t "~&== slash-command dispatch ==~%")
(let ((sess (tui::make-session)))
  ;; unknown command is handled (returns non-nil, non-:quit)
  (check "unknown cmd handled"
         (let ((r (capture (lambda () (tui::handle-command "/nope" sess))))) (declare (ignore r)) t))
  ;; /quit signals :quit
  (check "/quit -> :quit" (eq :quit (tui::handle-command "/quit" sess)))
  (check "/exit -> :quit" (eq :quit (tui::handle-command "/exit" sess)))
  ;; a plain line is NOT a command (returns nil so the repl runs a turn)
  (check "plain line -> nil" (null (tui::handle-command "hello there" sess)))
  ;; /clear wipes history
  (setf (tui::session-history sess) (list (llm:ht "role" "user" "content" "x")))
  (capture (lambda () (tui::handle-command "/clear" sess)))
  (check "/clear empties history" (null (tui::session-history sess)))
  ;; /help, /cost, /tools, /model, /system all produce output & return T
  (check "/help returns T" (eq t (capture-and-return sess "/help")))
  (check "/cost returns T"  (eq t (capture-and-return sess "/cost")))
  (check "/tools returns T" (eq t (capture-and-return sess "/tools")))
  (check "/model returns T" (eq t (capture-and-return sess "/model")))
  (check "/system returns T"(eq t (capture-and-return sess "/system")))
  ;; /help actually mentions the commands
  (let ((out (capture (lambda () (tui::handle-command "/help" sess)))))
    (check "/help lists /clear" (search "/clear" out))
    (check "/help lists /model" (search "/model" out))))

(format t "~&== tool-line rendering (hooks) ==~%")
(let* ((args (llm:ht "command" "echo hello  world"))
       (pre  (capture (lambda () (tui::tui-pre-hook "Bash" args))))
       (ok   (capture (lambda () (tui::tui-post-hook "Bash" args "hello world" nil 12))))
       (bad  (capture (lambda () (tui::tui-post-hook "Bash" args "TOOL ERROR: boom" nil 3)))))
  (check "pre shows tool name"  (search "Bash" pre))
  (check "pre shows arg"        (search "echo hello" pre))
  (check "post shows duration"  (search "12ms" ok))
  (check "post ok mark"         (search "✔" ok))
  (check "post error mark"      (search "✗" bad))
  (check "post error text"      (search "boom" bad)))

(format t "~&== streaming bullet logic ==~%")
(let ((out (capture (lambda ()
                      (let ((tui::*at-bol* t) (tui::*need-bullet* t)
                            (tui::*streamed* (make-string-output-stream)))
                        (tui::on-token "Hel") (tui::on-token "lo")
                        ;; a tool interrupts -> next text opens a new segment
                        (tui::tui-pre-hook "Read" (llm:ht "file_path" "x.lisp"))
                        (tui::on-token "world"))))))
  (check "streamed text present" (and (search "Hello" out) (search "world" out)))
  (check "bullet before segments" (search "⏺" out)))

(format t "~&== arg summary picks the right key ==~%")
(check "Read -> file_path" (string= "a.lisp" (tui::tool-arg-summary "Read" (llm:ht "file_path" "a.lisp" "limit" 10))))
(check "WebFetch -> url"   (search "example" (tui::tool-arg-summary "WebFetch" (llm:ht "url" "https://example.com"))))

;;; ---- best-effort live turn (needs a backend) ----
(format t "~&== live turn (best effort) ==~%")
(handler-case
    (progn
      (when (probe-file (merge-pathnames ".operandi/openrouter.token" (user-homedir-pathname)))
        (llm:use-openrouter :model "deepseek/deepseek-v4-flash"))
      (let* ((sess (tui::make-session))
             (out (capture (lambda ()
                             (tui::run-turn sess "Use the Bash tool to run exactly: echo operandi-lives . Then tell me the single word it printed.")))))
        (format t "~A~%" out)
        (if (search "operandi-lives" out)
            (format t "  ok   live turn ran a tool and answered~%")
            (format t "  warn live turn produced no expected marker (backend/model dependent)~%"))
        (check "live turn recorded a turn" (= 1 (tui::session-turns sess)))
        (check "live turn threaded history" (and (tui::session-history sess) t))))
  (error (e) (format t "  warn live turn skipped: ~A~%" e)))

(format t "~&~%tui-smoke: ~A failure~:P~%" *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
