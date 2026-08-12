;;;; nostr/nostr-test.lisp
;;;;
;;;; Offline oracle for operandi.nostr (the :operandi/nostr system). No network,
;;;; no LLM: generate agent + owner + stranger keypairs, gift-wrap messages with
;;;; cl-nostr's NIP-59, and drive on-giftwrap / process-item with the brain
;;;; (answer) and the relay publish stubbed. Proves the load-bearing contract:
;;;; only the cryptographically-authenticated operator is answered, stale/dup
;;;; messages are skipped, and the reply gift-wraps back so ONLY the operator
;;;; can read it (authenticated as the agent).
;;;;
;;;; NOT part of the core fitness suite (it needs cl-nostr, which the isolated
;;;; fitness registry doesn't carry). Run it directly:
;;;;   sbcl --non-interactive --eval '(asdf:load-system "operandi/nostr")' \
;;;;        --load nostr/nostr-test.lisp

(require :asdf)
(unless (find-package :ql) (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
(funcall (read-from-string "ql:quickload") "operandi/nostr" :silent t)

(defpackage #:operandi.nostr-test
  (:use #:cl)
  (:local-nicknames (#:na #:operandi.nostr) (#:keys #:cl-nostr.keys)
                    (#:nip59 #:cl-nostr.nip59) (#:pool #:cl-nostr.pool)))
(in-package #:operandi.nostr-test)

(defvar *fails* 0)
(defmacro check (name form)
  `(handler-case (if ,form (format t "  ok   ~A~%" ,name)
                     (progn (incf *fails*) (format t "  FAIL ~A~%" ,name)))
     (error (e) (incf *fails*) (format t "  ERR  ~A: ~A~%" ,name e))))

(defun sv (name) (symbol-value (find-symbol name :operandi.nostr)))
(defun (setf sv) (v name) (setf (symbol-value (find-symbol name :operandi.nostr)) v))
(defun call-na (name &rest args) (apply (find-symbol name :operandi.nostr) args))

;; isolate persistence to a scratch dir
(setf (sv "*KEY-FILE*")   (merge-pathnames "operandi-nostr-key"   (uiop:temporary-directory))
      (sv "*STATE-FILE*") (merge-pathnames "operandi-nostr-state" (uiop:temporary-directory))
      (sv "*TRANSCRIPT-FILE*") nil)   ; don't touch the real transcript during tests
(ignore-errors (delete-file (sv "*KEY-FILE*")))

;; identities — let ensure-agent-key MINT + PERSIST the agent (real persistence test)
(setf (sv "*AGENT-KP*") nil)
(call-na "ENSURE-AGENT-KEY")
(defparameter *agent* (sv "*AGENT-KP*"))
(defparameter *owner* (keys:generate-keypair))
(defparameter *stranger* (keys:generate-keypair))
(setf (sv "*OWNER-PUBKEY*") (keys:public-hex *owner*)
      (sv "*FLOOR*") 1000
      (sv "*QUEUE*") nil
      (sv "*SEEN*") (make-hash-table :test 'equal))

(defun ht (&rest kv) (apply (find-symbol "HT" :operandi.llm) kv))

(format t "~&== answer threads history: turn 2 gets the NEW message + prior context ==~%")
;; Regression for the 'Going well! How about you?' bug: eng:run IGNORES its prompt
;; when :history is set, so answer() must fold the new user message into history.
;; Stub eng:run to capture the messages it's handed, run two turns via REAL answer.
(let ((caps nil))
  (setf (sv "*HISTORY*") nil)
  (setf (fdefinition (find-symbol "RUN" :operandi.engine))
        (lambda (prompt &key history &allow-other-keys)
          (push history caps)
          (values (format nil "ack:~a" prompt)
                  (append (or history (list (ht "role" "system" "content" "s")
                                            (ht "role" "user" "content" prompt)))
                          (list (ht "role" "assistant" "content" "a"))))))
  (funcall (find-symbol "ANSWER" :operandi.nostr) "first question")
  (funcall (find-symbol "ANSWER" :operandi.nostr) "second question")
  (let ((turn2 (first caps)))          ; the :history eng:run got on turn 2
    (check "turn 2 threads a non-nil history" turn2)
    (check "turn 2's LAST message IS the new user text (not dropped)"
           (let ((m (car (last turn2))))
             (and (equal (gethash "role" m) "user") (equal (gethash "content" m) "second question"))))
    (check "turn 2 also carries turn 1's user message (context preserved)"
           (some (lambda (m) (equal (gethash "content" m) "first question")) turn2))))

;; stub the brain (no LLM) + capture the published reply (no network)
(defparameter *captured* nil)
(defparameter *captured-list* nil)
(setf (fdefinition (find-symbol "ANSWER" :operandi.nostr)) (lambda (text) (format nil "echo: ~a" text)))
(setf (fdefinition 'pool:pool-publish)
      (lambda (p e &rest r) (declare (ignore p r)) (push e *captured-list*) (setf *captured* e)))

(defun q () (sv "*QUEUE*"))
(defun wrap-from (kp text) (nip59:build-giftwrap (keys:keypair-secret-key kp) (keys:public-hex *agent*) text))

(format t "~&== identity: agent npub is stable + persisted ==~%")
(let ((npub1 (na:agent-npub)))
  (check "agent-npub is a valid npub1…" (and (stringp npub1) (>= (length npub1) 60)
                                             (string= (subseq npub1 0 5) "npub1")))
  (setf (sv "*AGENT-KP*") nil)
  (call-na "ENSURE-AGENT-KEY")
  (check "persisted key round-trips to the same npub" (string= npub1 (na:agent-npub))))

(format t "~&== only the authenticated operator is answered ==~%")
(call-na "ON-GIFTWRAP" (wrap-from *owner* "hello there") nil)
(check "an operator DM (newer than watermark) is enqueued" (= 1 (length (q))))
(call-na "ON-GIFTWRAP" (wrap-from *stranger* "let me in") nil)
(check "a STRANGER's DM is rejected (not enqueued)" (= 1 (length (q))))

(format t "~&== stale messages (<= watermark) are skipped ==~%")
(setf (sv "*FLOOR*") 9999999999)
(call-na "ON-GIFTWRAP" (wrap-from *owner* "old news") nil)
(check "a message at/under the watermark is skipped" (= 1 (length (q))))
(setf (sv "*FLOOR*") 1000)

(format t "~&== reply round-trips: only the operator can read it, authored by the agent ==~%")
(destructuring-bind (created text) (first (q))
  (let ((reply (call-na "PROCESS-ITEM" created text)))
    (check "process-item answered via the (stubbed) brain" (search "echo: hello there" reply))
    (check "a reply gift-wrap was published" *captured*)
    (multiple-value-bind (pt sender) (nip59:unwrap-giftwrap (keys:keypair-secret-key *owner*) *captured*)
      (check "operator unwraps the reply to the answer text" (search "echo: hello there" pt))
      (check "reply is authenticated as the AGENT" (equal sender (keys:public-hex *agent*))))
    (check "a stranger CANNOT unwrap the reply"
           (null (ignore-errors (nip59:unwrap-giftwrap (keys:keypair-secret-key *stranger*) *captured*))))))

(format t "~&== a long reply is SPLIT into ordered, numbered DMs (not truncated) ==~%")
(let* ((long (with-output-to-string (o)
               (dotimes (i 50) (format o "Paragraph ~a with several words to fill some space.~%~%" i))))
       (chunks (na:split-text long 200)))
  (check "split-text respects the max size" (every (lambda (c) (<= (length c) 200)) chunks))
  (check "split-text drops no non-whitespace content"
         (flet ((nw (s) (remove-if (lambda (c) (member c '(#\Space #\Newline #\Return #\Tab))) s)))
           (string= (nw long) (nw (apply #'concatenate 'string chunks)))))
  (check "split-text never cuts mid-word (chunks end at a boundary)"
         (every (lambda (c) (let ((n (length c))) (or (< n 200)
                                                      (member (char c (1- n)) '(#\. #\Space)))))
                (butlast chunks)))
  ;; send a long reply -> several ordered wraps the owner reads in sequence
  (setf *captured-list* nil (sv "*CHUNK-CHARS*") 200)
  (call-na "SEND-DM" long)
  (let ((wraps (reverse *captured-list*)))
    (check "a long reply becomes multiple DMs (no single-message truncation)" (> (length wraps) 1))
    (check "chunk created_at strictly increases (correct display order)"
           (apply #'< (mapcar #'cl-nostr.event:event-created-at wraps)))
    (let ((parts (mapcar (lambda (w) (nip59:unwrap-giftwrap (keys:keypair-secret-key *owner*) w)) wraps)))
      (check "every part is numbered (i/N)" (every (lambda (p) (and (find #\/ p) (find #\( p))) parts))))
  (setf (sv "*CHUNK-CHARS*") 1800))

(format t "~&== duplicate gift-wrap is processed once ==~%")
(setf (sv "*QUEUE*") nil (sv "*FLOOR*") 1000)
(let ((w (wrap-from *owner* "twice?")))
  (call-na "ON-GIFTWRAP" w nil)
  (call-na "ON-GIFTWRAP" w nil)
  (check "the same gift-wrap id enqueues only once" (= 1 (length (q)))))

(format t "~&~%operandi.nostr-test: ~A failure~:P~%" *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
