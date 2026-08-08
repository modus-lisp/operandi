;;; inspect/streaming-test.lisp
;;;
;;; Oracle for the SSE streaming path. The socket read is thin; the logic
;;; that matters — parsing 'data:' lines and assembling content + tool_calls
;;; from deltas into the blocking-path shape — is factored into parse-sse-
;;; line / sse-fold / sse-finalize and tested here with crafted events (no
;;; network). Also checks that *on-token* fires live as content arrives.
;;;
;;;   sbcl --non-interactive --load inspect/streaming-test.lisp   (0/1)

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.streaming-test
  (:use #:cl)
  (:local-nicknames (#:eng #:operandi.engine) (#:llm #:operandi.llm)))
(in-package #:operandi.streaming-test)

(defvar *pass* 0) (defvar *fail* 0)
(defmacro check (name &body body)
  `(handler-case
       (if (progn ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A~%" ,name)))
     (serious-condition (e)
       (incf *fail*) (format t "~&  FAIL ~A (~A)~%" ,name (type-of e)))))

(defun content-delta (s) (llm:ht "choices" (vector (llm:ht "delta" (llm:ht "content" s)))))
(defun tool-delta (index &key id name args)
  (let ((fn (llm:ht)))
    (when name (setf (gethash "name" fn) name))
    (when args (setf (gethash "arguments" fn) args))
    (let ((tc (llm:ht "index" index "function" fn)))
      (when id (setf (gethash "id" tc) id))
      (llm:ht "choices" (vector (llm:ht "delta" (llm:ht "tool_calls" (vector tc))))))))
(defun finish-delta (fr) (llm:ht "choices" (vector (llm:ht "delta" (llm:ht) "finish_reason" fr))))
(defun usage-delta (pt) (llm:ht "usage" (llm:ht "prompt_tokens" pt "completion_tokens" 3)))

(defun fold-all (events)
  (let ((st (eng::make-sse-state)))
    (dolist (e events) (eng::sse-fold st e))
    (eng::sse-finalize st)))

(defun msg-of (parsed)
  (gethash "message" (aref (gethash "choices" parsed) 0)))

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&operandi streaming (SSE assembly) oracle~%")

  (check "parse-sse-line: data + [DONE] + blanks/comments"
    (and (hash-table-p (eng::parse-sse-line "data: {\"a\":1}"))
         (hash-table-p (eng::parse-sse-line "data:{\"a\":1}"))     ; no space
         (eq :done (eng::parse-sse-line "data: [DONE]"))
         (null (eng::parse-sse-line ": keep-alive"))
         (null (eng::parse-sse-line ""))))

  (check "content deltas assemble into the full message"
    (let ((m (msg-of (fold-all (list (content-delta "Hel") (content-delta "lo")
                                     (finish-delta "stop"))))))
      (string= (gethash "content" m) "Hello")))

  (check "*on-token* fires live for each content delta"
    (let ((acc (make-string-output-stream)))
      (let ((eng:*on-token* (lambda (s) (write-string s acc))))
        (fold-all (list (content-delta "ab") (content-delta "cd"))))
      (string= (get-output-stream-string acc) "abcd")))

  (check "tool_call deltas assemble (name + fragmented arguments)"
    (let* ((m (msg-of (fold-all
                       (list (content-delta "")
                             (tool-delta 0 :id "call_1" :name "Read" :args "")
                             (tool-delta 0 :args "{\"path\":")
                             (tool-delta 0 :args "\"x\"}")
                             (finish-delta "tool_calls")))))
           (tcs (gethash "tool_calls" m))
           (tc  (and (vectorp tcs) (aref tcs 0)))
           (fn  (and tc (gethash "function" tc))))
      (and (= (length tcs) 1)
           (string= (gethash "id" tc) "call_1")
           (string= (gethash "name" fn) "Read")
           (string= (gethash "arguments" fn) "{\"path\":\"x\"}"))))

  (check "multiple tool_calls keep their index order"
    (let* ((m (msg-of (fold-all
                       (list (tool-delta 0 :id "c0" :name "A" :args "{}")
                             (tool-delta 1 :id "c1" :name "B" :args "{}")))))
           (tcs (gethash "tool_calls" m)))
      (and (= (length tcs) 2)
           (string= (gethash "name" (gethash "function" (aref tcs 0))) "A")
           (string= (gethash "name" (gethash "function" (aref tcs 1))) "B"))))

  (check "usage chunk is captured into the assembled response"
    (let ((parsed (fold-all (list (content-delta "x") (usage-delta 123)))))
      (= 123 (gethash "prompt_tokens" (gethash "usage" parsed)))))

  (check "assembled shape matches the blocking path (extract-message works)"
    (let ((m (eng::extract-message (fold-all (list (content-delta "done") (finish-delta "stop"))))))
      (and (hash-table-p m) (string= (gethash "content" m) "done"))))

  ;; --- OpenRouter 200-with-error-body streamed as an SSE event ---
  ;; This is the regression: an exhausted-grant / 402 arrives as a data: event
  ;; carrying {error:{...}}, often alongside a usage chunk. It must NOT finalize
  ;; as a blank-but-successful turn (which billed cents and returned "").
  (flet ((error-evt (msg code)
           (llm:ht "error" (llm:ht "message" msg "code" code))))
    (check "streamed error with no content finalizes as an error body (no choices)"
      (let ((parsed (fold-all (list (error-evt "Insufficient credits" 402)))))
        (and (eng::response-has-error-body-p parsed)
             (null (gethash "choices" parsed)))))

    (check "extract-message is NIL for a streamed error (no fake blank message)"
      (null (eng::extract-message
             (fold-all (list (error-evt "Insufficient credits" 402))))))

    (check "provider-error-text surfaces the real reason + code"
      (let ((txt (eng::provider-error-text
                  (fold-all (list (error-evt "Insufficient credits" 402))))))
        (and (search "Insufficient credits" txt) (search "402" txt))))

    (check "usage on an error event still rides the error body (so cost is attributable)"
      (let ((parsed (fold-all (list (error-evt "Insufficient credits" 402)
                                    (usage-delta 50)))))
        (and (eng::response-has-error-body-p parsed)
             (= 50 (gethash "prompt_tokens" (gethash "usage" parsed))))))

    (check "a real completion that merely mentions no error still wins over a spurious error field"
      ;; content present -> normal message, even if a stray error object appeared
      (let ((m (eng::extract-message
                (fold-all (list (content-delta "real answer") (error-evt "late warn" 0))))))
        (and (hash-table-p m) (string= (gethash "content" m) "real answer")))))

  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
