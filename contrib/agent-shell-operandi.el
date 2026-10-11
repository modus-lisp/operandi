;;; agent-shell-operandi.el --- Drive operandi from Emacs via agent-shell -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Registers operandi as an ACP agent in xenodium/agent-shell
;; (https://github.com/xenodium/agent-shell), so you can talk to it from
;; Emacs instead of the TUI.
;;
;; agent-shell spawns the agent with the project directory as its CWD, and
;; operandi's sandbox wrapper grants exactly that directory read/write.  So
;; launch agent-shell from the project you want operandi to work on.
;;
;; Setup:
;;
;;   (use-package agent-shell :ensure t)
;;   (load "/path/to/operandi/contrib/agent-shell-operandi.el")
;;   (setq agent-shell-preferred-agent-config 'operandi)   ; optional
;;
;; Then M-x agent-shell-operandi-start-agent, or M-x agent-shell and pick
;; "operandi".
;;
;; Model selection: the launcher reads OPERANDI_ACP_MODEL (an OpenRouter
;; model slug).  With no env var it falls back to deepseek-v4-flash when
;; ~/.operandi/openrouter.token exists, else the local llama server.  Set
;; `agent-shell-operandi-model' to override.

;;; Code:

(require 'agent-shell)

(defcustom agent-shell-operandi-acp-command
  (list (expand-file-name "bin/operandi-acp-sandbox-macos"
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory (or load-file-name buffer-file-name))))))
  "Command and parameters for the operandi ACP client.

The default points at the sandboxed launcher next to this file.  Point it
at bin/operandi-acp instead to run operandi without the macOS sandbox."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-operandi-model nil
  "OpenRouter model slug to run operandi with, or nil for the launcher default.

Passed to the agent as OPERANDI_ACP_MODEL, e.g. \"anthropic/claude-sonnet-4-5\"."
  :type '(choice (const :tag "Launcher default" nil) string)
  :group 'agent-shell)

(defcustom agent-shell-operandi-environment nil
  "Extra environment variables for the operandi client.

Build with `agent-shell-make-environment-variables'.  `agent-shell-operandi-model'
is applied on top of this."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-operandi--environment ()
  "Return the environment variable list for the operandi process."
  (append (when agent-shell-operandi-model
            (list (format "OPERANDI_ACP_MODEL=%s" agent-shell-operandi-model)))
          agent-shell-operandi-environment))

(defun agent-shell-operandi-make-client (buffer)
  "Create an operandi ACP client for BUFFER."
  (agent-shell--make-acp-client
   :command (car agent-shell-operandi-acp-command)
   :command-params (cdr agent-shell-operandi-acp-command)
   :environment-variables (agent-shell-operandi--environment)
   :context-buffer buffer))

(defun agent-shell-operandi-make-agent-config ()
  "Create an operandi agent configuration."
  (agent-shell-make-agent-config
   :identifier 'operandi
   :mode-line-name "operandi"
   :buffer-name "operandi"
   :shell-prompt "operandi> "
   :shell-prompt-regexp "operandi> "
   :welcome-function #'agent-shell-welcome-message
   :client-maker #'agent-shell-operandi-make-client
   :install-instructions
   "Build operandi's core image: run bin/operandi once from a terminal."))

;;;###autoload
(defun agent-shell-operandi-start-agent ()
  "Start an interactive operandi agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-operandi-make-agent-config)
                     :new-shell t))

;; Register with the agent picker, keeping the built-in agents.
(setq agent-shell-agent-configs
      (append (list #'agent-shell-operandi-make-agent-config)
              (agent-shell-default-agent-config-makers)))

(provide 'agent-shell-operandi)
;;; agent-shell-operandi.el ends here
