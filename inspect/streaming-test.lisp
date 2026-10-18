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
(defun reasoning-delta (s &key (key "reasoning_content"))
  (llm:ht "choices" (vector (llm:ht "delta" (llm:ht key s)))))

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

  ;; --- reasoning (the scratchpad) ---
  ;; A reasoning model emits delta.reasoning_content BEFORE any content, and on
  ;; a tool-calling turn it may emit nothing else. Dropping it is why the
  ;; commentary around tool use never appeared in the TUI.
  (check "reasoning deltas assemble onto the message (reasoning_content)"
    (let ((m (msg-of (fold-all (list (reasoning-delta "I should ")
                                     (reasoning-delta "list the files.")
                                     (finish-delta "tool_calls"))))))
      (string= (gethash "reasoning_content" m) "I should list the files.")))

  (check "reasoning deltas assemble from the OpenRouter 'reasoning' key too"
    (let ((m (msg-of (fold-all (list (reasoning-delta "think" :key "reasoning"))))))
      (string= (gethash "reasoning_content" m) "think")))

  (check "*on-reasoning* fires live for each reasoning delta"
    (let ((acc (make-string-output-stream)))
      (let ((eng:*on-reasoning* (lambda (s) (write-string s acc))))
        (fold-all (list (reasoning-delta "ab") (reasoning-delta "cd"))))
      (string= (get-output-stream-string acc) "abcd")))

  (check "reasoning does NOT fire *on-token* (it is not the answer)"
    (let ((acc (make-string-output-stream)))
      (let ((eng:*on-token* (lambda (s) (write-string s acc))))
        (fold-all (list (reasoning-delta "scratchpad"))))
      (zerop (length (get-output-stream-string acc)))))

  (check "a tool-call turn with ONLY reasoning still carries the trace"
    ;; the exact shape that produced the reported bug: no content at all
    (let* ((m (msg-of (fold-all (list (reasoning-delta "Extracting both parts.")
                                      (tool-delta 0 :id "c1" :name "Bash" :args "{}")
                                      (finish-delta "tool_calls")))))
           (tcs (gethash "tool_calls" m)))
      (and (string= (gethash "content" m) "")
           (string= (gethash "reasoning_content" m) "Extracting both parts.")
           (= (length tcs) 1))))

  (check "no reasoning key on the message when the model emitted none"
    (let ((m (msg-of (fold-all (list (content-delta "hi") (finish-delta "stop"))))))
      (null (gethash "reasoning_content" m))))

  (check "strip-reasoning removes the scratchpad before it is sent back"
    (let* ((msgs (vector (llm:ht "role" "assistant" "content" ""
                                 "reasoning_content" "scratch"
                                 "tool_calls" (vector (llm:ht "id" "c1")))
                         (llm:ht "role" "user" "content" "hi")))
           (out (eng::strip-reasoning msgs)))
      (and (null (gethash "reasoning_content" (aref out 0)))
           (string= (gethash "content" (aref out 0)) "")
           (gethash "tool_calls" (aref out 0))          ; everything else kept
           (string= (gethash "content" (aref out 1)) "hi"))))

  (check "strip-reasoning is identity (same object) when there is nothing to strip"
    (let ((msgs (vector (llm:ht "role" "user" "content" "hi"))))
      (eq msgs (eng::strip-reasoning msgs))))

  (check "render-turns shows the scratchpad as [thinking]"
    (let ((txt (eng::render-turns
                (list (llm:ht "role" "assistant" "content" ""
                              "reasoning_content" "Extracting both parts.")))))
      (and (search "[thinking] Extracting both parts." txt)
           (search "[assistant]" txt))))

  ;; --- the scratchpad must survive COMPACTION, not just the screen ---
  ;; Compaction is the mechanism that discards the middle of a conversation, and
  ;; on a tool-calling turn the trace is often the only place the model states
  ;; its intent.  If the summarizer never sees it, the fix above restores the
  ;; trace to the display and still loses it at the one boundary that matters.
  (check "the summarizer sees the scratchpad, labelled [thinking]"
    (let ((txt (eng::render-turn-for-summary
                (llm:ht "role" "assistant" "content" ""
                        "reasoning_content" "Extracting both parts."))))
      (and (search "[thinking] Extracting both parts." txt)
           (search "[assistant]" txt))))

  (check "the summarizer's copy of the scratchpad is clipped"
    (let* ((long (make-string 20000 :initial-element #\x))
           (txt (eng::render-turn-for-summary
                 (llm:ht "role" "assistant" "content" ""
                         "reasoning_content" long))))
      (and (< (length txt) 20000)
           (search "[truncated]" txt))))

  (check "the summarizer sees no [thinking] line when the model emitted none"
    (let ((txt (eng::render-turn-for-summary
                (llm:ht "role" "assistant" "content" "just an answer"))))
      (null (search "[thinking]" txt))))

  (check "the compaction prompt tells the summarizer what [thinking] is"
    (and (search "[thinking]" eng::*compaction-prompt*)
         (search "scratchpad" eng::*compaction-prompt*)))

  ;; chunk-turns sizes summarizer chunks.  ESTIMATE-TOKENS ignores the
  ;; scratchpad (it is stripped before the request goes out), so sizing by it
  ;; would under-count exactly the text the summarizer is now shown.
  (check "chunk-turns counts the scratchpad the summarizer will be shown"
    (let* ((m (llm:ht "role" "assistant" "content" ""
                      "reasoning_content" (make-string 40000 :initial-element #\x)))
           (by-estimate (eng::estimate-tokens (list m)))
           (by-summary  (ceiling (eng::summary-chars m) eng::*chars-per-token*)))
      (> by-summary by-estimate)))

  (check "chunk-turns splits a span that only overflows once the trace is counted"
    ;; 3 messages, each small by ESTIMATE-TOKENS but large once rendered.
    (let* ((msgs (loop repeat 3
                       collect (llm:ht "role" "assistant" "content" ""
                                       "reasoning_content"
                                       (make-string 40000 :initial-element #\x))))
           (chunks (eng::chunk-turns msgs 2000)))
      (> (length chunks) 1)))

  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
