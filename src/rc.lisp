;;;; rc.lisp — mirror each turn to a Marmot (MLS over Nostr) chat via cl-marmot.
;;;;
;;;; `--rc TARGET` on the command line, or `/rc TARGET` in the TUI, is RC:
;;;; an npub/nprofile/NIP-05 starts a NEW chat under an ephemeral per-run identity
;;;; (CREATE-CHAT-WITH); anything else names an EXISTING chat (ENABLE), opened from the
;;;; cl-marmot account in $MARMOT_HOME (default ~/.local/share/cl-marmot/), by name or
;;;; MLS group id.  After each
;;;; successful turn, MIRROR-TURN sends one chat message:  "> prompt" then a blank
;;;; line then the answer.
;;;;
;;;; cl-marmot is loaded at run time, not at build time, so operandi itself keeps
;;;; no dependency on it.  All marmot access goes through FIND-SYMBOL so this file
;;;; compiles whether or not cl-marmot is present.

(defpackage #:operandi.rc
  (:use #:cl)
  (:export #:rc #:enable #:create-chat-with #:disable #:active-p #:status-chat #:mirror-turn))

(in-package #:operandi.rc)

(defvar *client* nil "The open cl-marmot client, or NIL when not mirroring.")
(defvar *chat* nil "The chat designator given to ENABLE (name or group id), or NIL.")
(defvar *chat-name* nil "The resolved display name of *CHAT*.")

(defun %marmot (name)
  (or (find-symbol name "MARMOT")
      (error "cl-marmot does not export ~A" name)))

(defun %call (name &rest args)
  (apply (%marmot name) args))

(defun %modus-lisp-root ()
  "The directory holding the modus-lisp checkouts (cl-marmot and its deps), or NIL.
Same search order as cl-marmot/load-deps.lisp: $MODUS_LISP, ~/workspace/modus-lisp/, ~/modus-lisp/."
  (let* ((env (uiop:getenv "MODUS_LISP"))
         (candidates (remove nil
                             (list (and env (plusp (length env)) (uiop:ensure-directory-pathname env))
                                   (merge-pathnames "workspace/modus-lisp/" (user-homedir-pathname))
                                   (merge-pathnames "modus-lisp/" (user-homedir-pathname))))))
    (find-if (lambda (d) (probe-file (merge-pathnames "cl-marmot/cl-marmot.asd" d))) candidates)))

(defun %ensure-loaded ()
  "Make the MARMOT package available, loading cl-marmot and its modus-lisp deps on first use."
  (unless (find-package "MARMOT")
    (let ((root (%modus-lisp-root)))
      (unless root
        (error "cl-marmot checkout not found (set MODUS_LISP or put modus-lisp under ~~/workspace)"))
      (dolist (lib '("cl-marmot" "natrium" "secp256k1-fast" "seal" "cl-nostr" "json-simple"))
        (let ((dir (merge-pathnames (concatenate 'string lib "/") root)))
          (when (probe-file dir) (pushnew dir asdf:*central-registry* :test #'equal))))
      (funcall (read-from-string "ql:quickload") :cl-marmot :silent t)))
  (unless (find-package "MARMOT") (error "cl-marmot did not load"))
  t)

(defun %owed-welcomes (home)
  "Number of Welcomes still owed in the cl-marmot account at HOME (0 if none or unreadable)."
  (let ((acct (funcall (find-symbol "LOAD-ACCOUNT" "CL-MARMOT")
                       (concatenate 'string home "account.sexp"))))
    (length (funcall (find-symbol "ACCOUNT-OUTBOX" "CL-MARMOT") acct))))

(defun active-p () (and *client* *chat* t))

(defun status-chat () *chat-name*)

(defun disable ()
  "Stop mirroring and close the cl-marmot account.  Returns T if it was on."
  (let ((was (active-p)))
    (when *client*
      (ignore-errors (%call "CLOSE-ACCOUNT" *client*)))
    ;; The ephemeral identity's keys and group state go with the run, EXCEPT when a
    ;; Welcome is still owed (outbox non-empty): that home is the only copy, so keep
    ;; it and say where it is so delivery can be retried.
    (when *ephemeral-home*
      (let ((owed (ignore-errors (%owed-welcomes *ephemeral-home*))))
        (if (and owed (plusp owed))
            (format *error-output* "~&rc: ~D Welcome~:P still undelivered; kept identity in ~A~%"
                    owed *ephemeral-home*)
            (ignore-errors (uiop:delete-directory-tree (pathname *ephemeral-home*) :validate t)))))
    (setf *client* nil *chat* nil *chat-name* nil *ephemeral-home* nil)
    was))

(defun enable (chat)
  "Open the default cl-marmot account and mirror turns to CHAT (name or group id).
Signals an error with a readable message if cl-marmot, the account, or the chat is missing.
Returns the chat's display name."
  (check-type chat string)
  (%ensure-loaded)
  (disable)
  (let* ((home (%call "DEFAULT-HOME"))
         (client (progn
                   (unless (probe-file (concatenate 'string home "account.sexp"))
                     (error "no cl-marmot account in ~A (create one with cl-marmot's CLI first, or set MARMOT_HOME)"
                            home))
                   (handler-case (%call "OPEN-ACCOUNT")
                     (error (e) (error "cannot open the cl-marmot account in ~A: ~A" home e))))))
    (let* ((found (handler-case (%call "FIND-CHAT" client chat)
                    (error () nil))))
      (unless found
        (ignore-errors (%call "CLOSE-ACCOUNT" client))
        (error "no cl-marmot chat ~S (chats: ~{~A~^, ~})"
               chat
               (or (loop for c in (ignore-errors (%call "CHATS" client))
                         collect (let ((n (%call "CHAT-NAME" c)))
                                   (if (plusp (length n)) n (%call "CHAT-ID" c))))
                   '("none"))))
      (setf *client* client
            *chat* chat
            *chat-name* (let ((n (%call "CHAT-NAME" found)))
                          (if (plusp (length n)) n (%call "CHAT-ID" found)))))
    *chat-name*))

(defparameter *ephemeral-relays*
  '("wss://relay.damus.io" "wss://nos.lol" "wss://relay.primal.net"))

(defvar *ephemeral-home* nil "Temp MARMOT_HOME of the per-run identity, deleted on DISABLE.")

(defun %fresh-home ()
  (let ((dir (format nil "~A/operandi-rc-~36R-~36R/"
                     (string-right-trim "/" (or (uiop:getenv "TMPDIR") "/tmp"))
                     (get-universal-time) (random (expt 36 6)))))
    (ensure-directories-exist dir)
    dir))

(defun create-chat-with (invitee &key (name "") (relays *ephemeral-relays*))
  "Create a new chat with INVITEE (npub, nprofile, hex or NIP-05) under a fresh,
ephemeral cl-marmot identity in a temp home, and mirror turns to it.  The identity
is deleted by DISABLE; nothing persists in ~/.local/share/cl-marmot.  Returns the
chat's display name.  On failure the temp home is removed and an error is signalled."
  (check-type invitee string)
  (%ensure-loaded)
  (disable)
  (let* ((home (%fresh-home))
         (client nil))
    (handler-case
        (progn
          (setf client (%call "CREATE-ACCOUNT" :home home :relays relays
                              :name "operandi"))
          (let ((chat (%call "CREATE-CHAT" client (list invitee) :name name)))
            (setf *client* client
                  *ephemeral-home* home
                  *chat* (%call "CHAT-ID" chat)
                  *chat-name* (if (plusp (length name)) name invitee))
            *chat-name*))
      (error (e)
        (when client (ignore-errors (%call "CLOSE-ACCOUNT" client)))
        (ignore-errors (uiop:delete-directory-tree (pathname home) :validate t))
        (error "could not create a chat with ~A: ~A" invitee e)))))

(defun %new-target-p (target)
  "True when TARGET names a person (npub, nprofile or NIP-05), so RC starts a new chat.
A 64-hex string is never a person here: it can only name an existing group id."
  (let ((s (string-downcase target)))
    (or (uiop:string-prefix-p "npub1" s)
        (uiop:string-prefix-p "nprofile1" s)
        (and (find #\@ s) (not (find #\Space s))))))

(defun rc (target)
  "Mirror turns to TARGET.  An npub, nprofile or NIP-05 starts a NEW chat with that
person under an ephemeral identity (CREATE-CHAT-WITH).  Anything else (a chat name or
group id, including hex) mirrors to an EXISTING chat (ENABLE).  Returns the chat name."
  (if (%new-target-p target)
      (create-chat-with target)
      (enable target)))

(defun mirror-turn (prompt answer)
  "Send one chat message for a finished turn: \"> PROMPT\", a blank line, then ANSWER.
Does nothing when not mirroring.  Returns NIL on success, or a message string on failure
(the caller prints it; a failed mirror never fails the turn)."
  (when (active-p)
    (handler-case
        (let ((text (format nil "> ~A~%~%~A"
                            (string-trim '(#\Space #\Newline) (or prompt ""))
                            (string-trim '(#\Space #\Newline) (or answer "")))))
          (%call "SEND" *client* *chat* (string-right-trim '(#\Space #\Newline) text))
          nil)
      (error (e) (format nil "mirror to ~A failed: ~A" *chat-name* e)))))
