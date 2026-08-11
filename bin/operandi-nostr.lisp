;;; bin/operandi-nostr.lisp
;;;
;;; Headless operandi agent over Nostr private DMs (NIP-17). It answers ONLY the
;;; operator — identified by a NIP-05 address — and replies gift-wrapped so only
;;; the operator can read them. See the operandi/nostr system (nostr/nostr.lisp).
;;;
;;;   sbcl --non-interactive --load bin/operandi-nostr.lisp
;;;
;;; Config via env:
;;;   OPERANDI_NOSTR_NIP05    operator NIP-05 address (REQUIRED, e.g. you@example.com)
;;;   OPERANDI_NOSTR_MODEL    OpenRouter model        (default deepseek/deepseek-v4-flash)
;;;   OPERANDI_NOSTR_RELAYS   comma-separated relay URLs (added to the defaults)
;;;
;;; Blocks and serves; front with a flock + respawn supervisor for unattended runs.

(require :asdf)
(let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file q) (load q)))
(funcall (read-from-string "ql:quickload") "operandi/nostr" :silent t)

(in-package #:operandi.nostr)

(let ((nip05  (uiop:getenv "OPERANDI_NOSTR_NIP05"))
      (model  (or (uiop:getenv "OPERANDI_NOSTR_MODEL") *model*))
      (extra  (uiop:getenv "OPERANDI_NOSTR_RELAYS")))
  (unless nip05
    (format *error-output* "~&operandi-nostr: set OPERANDI_NOSTR_NIP05 to the operator's NIP-05 address (e.g. you@example.com).~%")
    (sb-ext:exit :code 2))
  (when extra
    (setf *relays* (remove-duplicates
                    (append *relays* (uiop:split-string extra :separator ","))
                    :test #'equal)))
  (setf *owner-nip05* nip05 *model* model)
  ;; greet the operator on startup unless OPERANDI_NOSTR_GREET is 0/no/off
  (let ((greet (let ((g (uiop:getenv "OPERANDI_NOSTR_GREET")))
                 (not (member g '("0" "no" "off" "false") :test #'equalp)))))
    (run-loop :nip05 nip05 :model model :greet greet)))
