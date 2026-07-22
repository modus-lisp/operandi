;;; inspect/session-test.lisp
;;;
;;; Oracle for TUI session persistence + --resume (operandi.tui). Deterministic,
;;; no network: build a session with a tool-calling history, persist it, resume
;;; into a FRESH session, and assert the raw message history round-trips intact
;;; (roles, content, and the tool_calls <-> tool_call_id linkage that makes the
;;; restored context a valid thing to re-send). Also covers listing, resume
;;; :latest, graceful missing-id, and that a session is a redefine-safe
;;; hash-table (not a struct).
;;;
;;; Exit 0 iff all checks pass; doubles as a fitness/swarm oracle.

(require :asdf)
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:session-test
  (:use #:cl)
  (:local-nicknames (#:tui #:operandi.tui) (#:session #:operandi.session) (#:llm #:operandi.llm)))
(in-package #:session-test)

(defvar *fails* 0)
(defmacro check (name form)
  `(handler-case (if ,form (format t "  ok   ~A~%" ,name)
                     (progn (incf *fails*) (format t "  FAIL ~A~%" ,name)))
     (error (e) (incf *fails*) (format t "  ERR  ~A: ~A~%" ,name e))))

;; isolate all writes to a scratch dir
(setf (symbol-value (find-symbol "*SESSIONS-DIR*" "OPERANDI.SESSION"))
      (namestring (merge-pathnames "operandi-session-test/" (uiop:temporary-directory))))
(ignore-errors (uiop:delete-directory-tree
                (pathname (symbol-value (find-symbol "*SESSIONS-DIR*" "OPERANDI.SESSION")))
                :validate t :if-does-not-exist :ignore))

(defun mk (&rest kv) (apply #'llm:ht kv))
(defun sref (s k) (gethash k s))

(format t "~&== session is a redefine-safe hash-table ==~%")
(let ((s (session::make-session)))
  (check "make-session returns a hash-table" (hash-table-p s))
  (check "fresh id looks like YYYYMMDD-HHMMSS"
         (let ((id (sref s :id))) (and (= (length id) 15) (char= (char id 8) #\-))))
  (check "reset-session! assigns a NEW id"
         ;; ids are second-resolution; force a difference by stuffing one
         (let ((old (sref s :id)))
           (setf (gethash :id s) "00000000-000000")
           (session::reset-session! s)
           (not (string= (sref s :id) "00000000-000000")))))

(format t "~&== persist -> resume round-trips the raw history ==~%")
(let ((s (session::make-session)))
  (setf (gethash :history s)
        (list (mk "role" "system" "content" "SYS")
              (mk "role" "user" "content" "run echo hi")
              (mk "role" "assistant" "content" "ok"
                  "tool_calls" (vector (mk "id" "call_1" "type" "function"
                                           "function" (mk "name" "Bash"
                                                          "arguments" "{\"command\":\"echo hi\"}"))))
              (mk "role" "tool" "tool_call_id" "call_1" "content" "hi")
              (mk "role" "assistant" "content" "It printed hi."))
        (gethash :turns s) 2 (gethash :cost s) 0.0123d0 (gethash :calls s) 3
        (gethash :prompt-tok s) 500 (gethash :completion-tok s) 20)
  (check "persist-session writes files" (session::persist-session s))
  (let* ((id (gethash :id s))
         (r (session::make-session))
         (got (session::resume-session! r id)))
    (check "resume returns the id"        (equal got id))
    (check "turns restored"               (= 2 (gethash :turns r)))
    (check "calls restored"               (= 3 (gethash :calls r)))
    (check "cost restored"                (< (abs (- 0.0123d0 (gethash :cost r))) 1d-9))
    (let ((h (gethash :history r)))
      (check "history length preserved"   (= 5 (length h)))
      (check "roles preserved in order"
             (equal '("system" "user" "assistant" "tool" "assistant")
                    (mapcar (lambda (m) (gethash "role" m)) h)))
      (let* ((asst (third h)) (tcs (gethash "tool_calls" asst))
             (tc (and tcs (> (length tcs) 0) (elt tcs 0)))
             (fn (and tc (gethash "function" tc))))
        (check "assistant content preserved" (equal "ok" (gethash "content" asst)))
        (check "tool_calls preserved"        (and fn (equal "Bash" (gethash "name" fn))))
        (check "tool args preserved"         (and fn (search "echo hi" (gethash "arguments" fn))))
        (check "tool_call_id linkage intact"
               (equal "call_1" (gethash "tool_call_id" (fourth h))))))))

(format t "~&== listing, latest, and graceful misses ==~%")
(let ((rows (session::list-sessions)))
  (check "list-sessions finds the saved session" (and rows (>= (length rows) 1)))
  (check "list row is (id turns first-prompt)"
         (and (stringp (first (first rows))) (integerp (second (first rows))))))
(let ((r (session::make-session)))
  (check "resume :latest resolves to newest" (stringp (session::resume-session! r :latest))))
(let ((r (session::make-session)))
  (check "resume of a missing id returns NIL"
         (null (session::resume-session! r "19990101-000000"))))

(format t "~&~%session-test: ~A failure~:P~%" *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
