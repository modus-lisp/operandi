;;; inspect/compaction-test.lisp
;;;
;;; Oracle for token-aware context compaction. Verifies the trigger is
;;; token-driven, tier-1 (trim old tool results) reclaims space without
;;; an LLM call, tier-2 (LLM summary) kicks in only when trimming isn't
;;; enough, and the head / recent tail / tool-call protocol survive.
;;;
;;;   sbcl --non-interactive --load inspect/compaction-test.lisp  (0/1)

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.compaction-test
  (:use #:cl)
  (:local-nicknames (#:eng #:operandi.engine) (#:llm #:operandi.llm)))
(in-package #:operandi.compaction-test)

(defvar *pass* 0) (defvar *fail* 0) (defvar *summ-called* nil)

(defmacro check (name &body body)
  `(handler-case
       (if (progn ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A~%" ,name)))
     (serious-condition (e)
       (incf *fail*) (format t "~&  FAIL ~A (~A)~%" ,name (type-of e)))))

(defun sys (s) (llm:ht "role" "system" "content" s))
(defun usr (s) (llm:ht "role" "user" "content" s))
(defun asst (s) (llm:ht "role" "assistant" "content" s))
(defun toolm (id content) (llm:ht "role" "tool" "tool_call_id" id "content" content))
(defun asst-tc (id cmd)
  (llm:ht "role" "assistant" "content" ""
          "tool_calls"
          (vector (llm:ht "id" id "type" "function"
                          "function" (llm:ht "name" "Bash"
                                             "arguments" (format nil "{\"command\":~S}" cmd))))))
(defun big (n) (make-string n :initial-element #\a))

(defun convo (&key (pairs 4) (toolsize 40000))
  "system + user, then PAIRS of (assistant tool-call, big tool result),
   then a small recent tail."
  (append (list (sys "system prompt") (usr "the task"))
          (loop for k below pairs
                append (list (asst-tc (format nil "c~D" k) "run something")
                             (toolm (format nil "c~D" k) (big toolsize))))
          (list (asst "recent thought") (usr "keep going"))))

(defun tool-msg-count (ms)
  (count "tool" ms :key (lambda (m) (gethash "role" m)) :test #'string=))
(defun max-tool-len (ms)
  (loop for m in ms
        when (string= (gethash "role" m) "tool")
          maximize (length (gethash "content" m))))

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&operandi token-aware compaction oracle~%")

  ;; token estimate is in the right ballpark (~4 chars/token)
  (check "estimate-tokens ~ chars/4"
    (let ((tok (eng:estimate-tokens (list (asst (big 4000))))))
      (and (>= tok 950) (<= tok 1100))))

  ;; the estimator is pluggable (a deployment can swap in a real tokenizer)
  (check "token estimator is pluggable"
    (let ((eng:*token-estimator* (lambda (ms) (declare (ignore ms)) 42)))
      (= 42 (eng:estimate-tokens (list (asst "anything"))))))

  ;; calibration nudges chars-per-token toward the observed ratio
  (check "calibration adapts chars-per-token to observed usage"
    (let ((eng:*chars-per-token* 4.0d0))
      (eng::calibrate-chars-per-token 3000 1000)   ; observed 3.0 chars/token
      (and (< eng:*chars-per-token* 4.0d0) (> eng:*chars-per-token* 3.0d0))))

  ;; under budget => untouched
  (check "under budget: no compaction"
    (let ((eng:*context-token-budget* 1000000)
          (ms (convo)))
      (eq ms (eng::maybe-compact ms nil))))

  ;; tier 1: trimming old tool results alone gets under budget, NO LLM call
  (check "tier-1 trims old tool results, no LLM summary"
    (let ((eng:*context-token-budget* 15000)
          (eng:*compact-keep-last* 2)
          (*summ-called* nil))
      (setf (symbol-function 'operandi.engine::summarize-turns)
            (lambda (turns) (declare (ignore turns)) (setf *summ-called* t) "SUMMARY"))
      (let ((out (eng::compact-messages (convo :pairs 4 :toolsize 40000))))
        (and (<= (eng:estimate-tokens out) 15000)     ; under budget
             (not *summ-called*)                        ; tier-1 only
             (= (tool-msg-count out) 4)                 ; results kept, not dropped
             (< (max-tool-len out) 2000)))))            ; but trimmed

  ;; trimmed tool messages keep their tool_call_id (protocol integrity)
  (check "tier-1 preserves tool_call_id"
    (let ((eng:*context-token-budget* 15000) (eng:*compact-keep-last* 2))
      (let ((out (eng::trim-old-tool-results (convo :pairs 3 :toolsize 40000) 2)))
        (every (lambda (m) (or (not (string= (gethash "role" m) "tool"))
                               (gethash "tool_call_id" m)))
               out))))

  ;; tier 2: when trimming isn't enough, the LLM summary runs
  (check "tier-2 LLM summary when trim insufficient"
    (let ((eng:*context-token-budget* 400)
          (eng:*compact-keep-last* 2)
          (*summ-called* nil))
      (setf (symbol-function 'operandi.engine::summarize-turns)
            (lambda (turns) (declare (ignore turns)) (setf *summ-called* t) "SUMMARY"))
      (let ((out (eng::compact-messages (convo :pairs 4 :toolsize 40000))))
        (and *summ-called*
             (some (lambda (m) (and (stringp (gethash "content" m))
                                    (search "SUMMARY" (gethash "content" m))))
                   out)))))

  ;; tier-2 is REVERSIBLE: the raw middle is offloaded to a readable file,
  ;; not destroyed — the agent can Read it back.
  (check "tier-2 offloads the raw middle to a file (reversible)"
    (let* ((dir (namestring (merge-pathnames "operandi-offload-test/"
                                             (uiop:temporary-directory))))
           (eng:*context-token-budget* 400)
           (eng:*compact-keep-last* 2)
           (eng:*offload-dir* dir))
      (uiop:delete-directory-tree (pathname dir) :validate t :if-does-not-exist :ignore)
      (setf (symbol-function 'operandi.engine::summarize-turns)
            (lambda (turns) (declare (ignore turns)) "SUMMARY"))
      (eng::compact-messages (convo :pairs 4 :toolsize 4000))
      (let ((files (directory (merge-pathnames "*.txt" (pathname dir)))))
        (and files
             ;; the offloaded file contains the raw tool-result content ('a's)
             (search "aaaa" (uiop:read-file-string (first files)))))))

  ;; head (system + first user) is always preserved verbatim
  (check "head preserved through compaction"
    (let ((eng:*context-token-budget* 400) (eng:*compact-keep-last* 2))
      (setf (symbol-function 'operandi.engine::summarize-turns)
            (lambda (turns) (declare (ignore turns)) "SUMMARY"))
      (let* ((ms (convo)) (out (eng::compact-messages ms)))
        (and (eq (first ms) (first out)) (eq (second ms) (second out))))))

  ;; tail never begins on a tool message (would orphan it from its call)
  (check "safe-tail-start never lands on a tool message"
    (let* ((ms (convo :pairs 4 :toolsize 100))
           (idx (operandi.engine::safe-tail-start ms 3)))
      (not (string= (gethash "role" (nth idx ms)) "tool"))))

  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
