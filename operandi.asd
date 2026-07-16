;;;; operandi.asd
;;;;
;;;; operandi — a Lisp-native ReAct agent loop ("Claude Code in Lisp").
;;;;
;;;; A small local LLM or a frontier model (via OpenRouter) drives a
;;;; tool-calling loop against the OpenAI-compatible chat-completions
;;;; protocol: read/write/edit files, run shell, search code and the
;;;; web, and — the sharp edge — Eval arbitrary Common Lisp in the
;;;; running SBCL image, so the agent can call straight into whatever
;;;; domain packages the host application has loaded.
;;;;
;;;; Load with:  (asdf:load-system :operandi)   ; or (ql:quickload :operandi)
;;;; then run:   (operandi.engine:run "your task here")

(asdf:defsystem "operandi"
  :description "Lisp-native ReAct agent loop with an Eval-into-the-image tool."
  :author "ynniv"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("com.inuoe.jzon"
               "dexador"
               "cl-ppcre"
               "sqlite"
               "bordeaux-threads")
  :serial nil
  :components
  ((:module "src"
    :components
    ((:file "llm")                                            ; operandi.llm
     (:file "store")                                          ; operandi.store
     (:file "search")                                         ; operandi.search
     (:file "safefetch" :depends-on ("llm"))                  ; operandi.safefetch
     (:file "hooks"    :depends-on ("store"))                 ; operandi.hooks
     (:file "tools"    :depends-on ("llm" "search" "hooks" "safefetch"))  ; operandi.tools
     (:file "engine"   :depends-on ("llm" "tools" "hooks" "safefetch"))   ; operandi.engine
     (:file "subagent" :depends-on ("llm" "tools" "engine"))  ; operandi.subagent
     (:file "cron"     :depends-on ("engine"))                ; operandi.cron
     (:file "tui"      :depends-on ("engine" "tools" "llm" "hooks"))))))  ; operandi.tui
