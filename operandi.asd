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
    ((:file "text")                                           ; operandi.text
     (:file "llm")                                            ; operandi.llm
     (:file "store")                                          ; operandi.store
     (:file "search")                                         ; operandi.search
     (:file "safefetch" :depends-on ("llm" "text"))           ; operandi.safefetch
     (:file "hooks"    :depends-on ("store"))                 ; operandi.hooks
     (:file "tools"    :depends-on ("llm" "search" "hooks" "safefetch" "text"))  ; operandi.tools
     (:file "engine"   :depends-on ("llm" "tools" "hooks" "safefetch"))   ; operandi.engine
     (:file "subagent" :depends-on ("llm" "tools" "engine"))  ; operandi.subagent
     (:file "cron"     :depends-on ("engine"))                ; operandi.cron
     (:file "session"  :depends-on ("llm"))                   ; operandi.session
     (:file "sessiontree")                                    ; operandi.sessiontree
     (:file "tui"      :depends-on ("engine" "tools" "llm" "hooks" "session"))  ; operandi.tui
     (:file "acp"      :depends-on ("engine" "tools" "llm" "hooks" "session"))))))  ; operandi.acp

;;; Optional: a headless agent that chats over Nostr private DMs (NIP-17). Kept a
;;; SEPARATE system so the core stays dependency-light — this one pulls cl-nostr
;;; (github.com/modus-lisp/cl-nostr) for the wire mechanics.
;;;   (asdf:load-system "operandi/nostr")  ; then (operandi.nostr:run-loop :nip05 "you@example.com")
(asdf:defsystem "operandi/nostr"
  :description "Headless operandi agent over Nostr NIP-17 private DMs."
  :author "ynniv"
  :license "MIT"
  :depends-on ("operandi" "cl-nostr" "bordeaux-threads")
  :pathname "nostr"
  :serial t
  :components ((:file "nostr")))                              ; operandi.nostr
