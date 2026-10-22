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
  :depends-on ("json-simple"
               "seal/http"      ; pure-CL HTTP/TLS (on natrium) -- replaced dexador, which was
                                ; the only way FFI entered this stack (cl+ssl, iolib, static-vectors)
               "cl-sev-snp"     ; AMD SEV-SNP / AWS Nitro attestation verification, pure CL --
                                ; what makes :PPQ --confidential-tinfoil a local trust decision, not a promise
               "cl-sev-snp/kds" ; ...plus the AMD KDS fetch for the ARK/ASK chain
               "babel"
               "quri"
               "cl-base64"
               "cl-ppcre"
               "sqlite"
               "bordeaux-threads"
               "lispqr"         ; pure-CL QR encoder (mare5x, MIT) -- for payment invoices; pulls zpng
               "png-pure"       ; pure-CL PNG encoder (cram's deflate) -- QR image for :open
               "quill")         ; pure-CL line editor for the REPL prompt -- replaced linedit
  :serial nil
  :components
  ((:module "src"
    :components
    ((:file "text")                                           ; operandi.text
     (:file "http")                                           ; operandi.http
     (:file "payments" :depends-on ("http"))                  ; operandi.payments
     (:file "ppq"      :depends-on ("http"))                  ; operandi.ppq
     (:file "llm"      :depends-on ("http" "ppq"))            ; operandi.llm
     (:file "actor")                                          ; operandi.actor
     (:file "store")                                          ; operandi.store
     (:file "search"   :depends-on ("http" "llm"))            ; operandi.search
     (:file "safefetch" :depends-on ("http" "llm" "text"))    ; operandi.safefetch
     (:file "hooks"    :depends-on ("store"))                 ; operandi.hooks
     (:file "tools"    :depends-on ("llm" "search" "hooks" "safefetch" "text"))  ; operandi.tools
     (:file "mcp"      :depends-on ("http" "llm" "tools" "text"))                ; operandi.mcp (MCP client)
     (:file "engine"   :depends-on ("http" "llm" "tools" "hooks" "safefetch"))   ; operandi.engine
     (:file "subagent" :depends-on ("llm" "tools" "engine"))  ; operandi.subagent
     (:file "cron"     :depends-on ("engine"))                ; operandi.cron
     (:file "session"  :depends-on ("llm"))                   ; operandi.session
     (:file "sessiontree")                                    ; operandi.sessiontree
     (:file "rc"       :depends-on ("store"))                 ; operandi.rc (Marmot mirror; cl-marmot loaded at run time)
     (:file "tui"      :depends-on ("engine" "tools" "llm" "hooks" "session" "subagent" "rc"))  ; operandi.tui
     (:file "acp"      :depends-on ("actor" "store" "engine" "tools" "llm" "hooks" "session"))))))  ; operandi.acp

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
