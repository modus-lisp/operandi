;;; bin/operandi-acp.lisp
;;;
;;; Entry point for the operandi ACP server (agentclientprotocol.com): editors
;;; like Zed spawn this and speak JSON-RPC 2.0 over its stdin/stdout.
;;;
;;;   sbcl --non-interactive --load bin/operandi-acp.lisp
;;;
;;; STDOUT is the protocol channel — nothing but JSON-RPC may touch it. So all
;;; loading/setup output is routed to stderr, and the real stdout is captured
;;; and handed to SERVE.
;;;
;;; Backend: OPERANDI_ACP_MODEL env var picks an OpenRouter model; else, if an
;;; OpenRouter token exists it defaults to deepseek-v4-flash; else local llama.

(require :asdf)
(unless (find-package :ql)
  (let ((init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file init) (load init))))

(defparameter cl-user::*acp-stdout* *standard-output*)   ; the protocol channel

(let ((*standard-output* *error-output*))                ; keep stdout clean during setup
  (unless (find-package :operandi.acp)
    (handler-case (funcall (read-from-string "ql:quickload") :operandi :silent t)
      (error () (asdf:load-system :operandi))))
  (funcall (find-symbol "OPEN-STORE" "OPERANDI.STORE"))
  (let ((model (uiop:getenv "OPERANDI_ACP_MODEL"))
        (tok (merge-pathnames ".operandi/openrouter.token" (user-homedir-pathname))))
    (cond
      (model (funcall (find-symbol "USE-OPENROUTER" "OPERANDI.LLM") :model model))
      ((probe-file tok)
       (funcall (find-symbol "USE-OPENROUTER" "OPERANDI.LLM")
                :model "deepseek/deepseek-v4-flash")))))

;; Serve until the client closes stdin. JSON-RPC goes to the captured stdout.
(funcall (find-symbol "SERVE" "OPERANDI.ACP") :out cl-user::*acp-stdout*)
(sb-ext:exit :code 0)
