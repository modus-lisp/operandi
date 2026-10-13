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
;;;; cl-marmot is loaded at run time (ql:quickload), not at build time, so operandi keeps
;;;; no dependency on it.  All marmot access goes through FIND-SYMBOL so this file
;;;; compiles whether or not cl-marmot is present.

(defpackage #:operandi.rc
  (:use #:cl)
  (:export #:rc #:enable #:create-chat-with #:disable #:active-p #:status-chat #:mirror-turn
           #:set-inbound #:*mention*))

(in-package #:operandi.rc)

(defvar *client* nil "The open cl-marmot client, or NIL when not mirroring.")
(defvar *ephemeral-home* nil "Temp MARMOT_HOME of the per-run identity, deleted on DISABLE.")
(defvar *chat* nil "The chat designator given to ENABLE (name or group id), or NIL.")
(defvar *chat-name* nil "The resolved display name of *CHAT*.")

(defun %marmot (name)
  (or (find-symbol name "MARMOT")
      (error "cl-marmot does not export ~A" name)))

(defun %call (name &rest args)
  (apply (%marmot name) args))

(defun %ensure-loaded ()
  "Make the MARMOT package available. cl-marmot and its deps are quickloaded from
~/quicklisp/local-projects (each is a symlink to its checkout), so no path search here."
  (unless (find-package "MARMOT")
    (funcall (read-from-string "ql:quickload") :cl-marmot :silent t))
  (unless (find-package "MARMOT") (error "cl-marmot did not load"))
  t)

(defun %owed-welcomes (home)
  "Number of Welcomes still owed in the cl-marmot account at HOME (0 if none or unreadable)."
  (let ((acct (funcall (find-symbol "LOAD-ACCOUNT" "CL-MARMOT")
                       (concatenate 'string home "account.sexp"))))
    (length (funcall (find-symbol "ACCOUNT-OUTBOX" "CL-MARMOT") acct))))

(defvar *on-inbound* nil
  "Called with the message text (the part after the @operandi mention is NOT stripped)
when a chat member sends a message that mentions @operandi.  The TUI sets this to
queue a turn.  NIL means inbound messages are ignored.")

(defvar *mention* nil
  "The token a chat message must contain to drive a turn: \"@\" plus this run's
ephemeral id (see %NEW-MENTION).  NIL when not mirroring.")

(defun %new-mention ()
  "A fresh per-run mention token, e.g. \"@op-7k3q9z\".  Random, so each run's id is unique."
  (format nil "@op-~(~36R~)" (random (expt 36 6) (make-random-state t))))

(defun %mentions-us-p (content npub)
  "True if CONTENT addresses us: it contains *MENTION* or our NPUB (WhiteNoise rewrites
a typed @token into an @npub1… mention, so both forms count)."
  (or (and *mention* (search *mention* content :test #'char-equal))
      (and npub (search npub content :test #'char-equal))))

(defun %inbound-text (content &optional npub)
  "The message CONTENT if it is an inbound request: it mentions us (see %MENTIONS-US-P)
and is not one of our own '> prompt' mirrors.  NIL otherwise."
  (when (and (stringp content) *mention*
             (not (uiop:string-prefix-p ">" (string-left-trim '(#\Space #\Tab #\Newline) content)))
             (%mentions-us-p content npub))
    (string-trim '(#\Space #\Tab #\Newline) content)))

(defvar *pending-inbound* '()
  "Inbound requests that arrived before a consumer was attached (cl-marmot's sync
consumes a message once, so it must not be dropped).  Oldest first.")

(defvar *inbound-lock* (bt:make-lock "rc-inbound"))

(defun %deliver-inbound (text)
  "Hand TEXT to *ON-INBOUND*, or hold it in *PENDING-INBOUND* until a consumer exists."
  (let ((fn (bt:with-lock-held (*inbound-lock*)
              (or *on-inbound*
                  (progn (setf *pending-inbound* (append *pending-inbound* (list text)))
                         nil)))))
    (when fn (funcall fn text))))

(defun set-inbound (fn)
  "Attach FN as the inbound consumer and flush anything that arrived before it."
  (let ((backlog (bt:with-lock-held (*inbound-lock*)
                   (setf *on-inbound* fn)
                   (prog1 *pending-inbound* (setf *pending-inbound* '())))))
    (when fn (dolist (text backlog) (funcall fn text)))
    fn))

(defun %on-event (event)
  "cl-marmot handler: forward inbound chat messages that mention *MENTION* on.
Our own messages are skipped (they are the mirrors)."
  (let ((msg (funcall (%marmot "EVENT-MESSAGE") event)))
    (when (and (eq (funcall (%marmot "EVENT-TYPE") event) :message)
               msg
               (not (funcall (%marmot "MESSAGE-OWN-P") msg)))
      (let ((text (%inbound-text (funcall (%marmot "MESSAGE-CONTENT") msg)
                                 (ignore-errors (%call "NPUB" *client*)))))
        (when text (%deliver-inbound text))))))

(defun %start-sync ()
  "Run cl-marmot's background sync on the open client so inbound messages arrive."
  (%call "ADD-HANDLER" *client* #'%on-event)
  (%call "START" *client* :serve nil))

(defun active-p () (and *client* *chat* t))

(defun status-chat () *chat-name*)

(defun disable ()
  "Stop mirroring and close the cl-marmot account.  Returns T if it was on."
  (let ((was (active-p)))
    (when *client*
      (ignore-errors (%call "STOP" *client*))
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
    (setf *client* nil *chat* nil *chat-name* nil *ephemeral-home* nil *mention* nil)
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
            *chat* found
            *chat-name* (let ((n (%call "CHAT-NAME" found)))
                          (if (plusp (length n)) n (%call "CHAT-ID" found)))
            *mention* (%new-mention))
      (%start-sync))
    *chat-name*))

(defparameter *ephemeral-relays*
  '("wss://relay.damus.io" "wss://nos.lol" "wss://relay.primal.net"))

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
                  *chat-name* (if (plusp (length name)) name invitee)
                  *mention* (%new-mention))
            (%start-sync)
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
