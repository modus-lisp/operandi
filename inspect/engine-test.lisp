;;; inspect/engine-test.lisp
;;;
;;; Oracle for the agent loop's handling of empty / null-content turns.
;;; The chat API sometimes ends a turn with content=null and no tool
;;; call (a stall); jzon parses that null to the symbol NULL, which used
;;; to leak out as the literal answer "NULL". The loop should instead
;;; nudge the model back to work and, failing that, salvage the last real
;;; content — never return "NULL"/"".
;;;
;;; We stub DO-CHAT-WITH-RETRIES with a scripted queue of responses, so
;;; the whole loop runs deterministically with no network.
;;;
;;;   sbcl --non-interactive --load inspect/engine-test.lisp   (exits 0/1)

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.engine-test
  (:use #:cl)
  (:local-nicknames (#:eng #:operandi.engine)
                    (#:llm #:operandi.llm))
  (:export #:run))
(in-package #:operandi.engine-test)

(defvar *pass* 0)
(defvar *fail* 0)
(defvar *script* nil)

(defmacro check (name &body body)
  `(handler-case
       (if (progn ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A~%" ,name)))
     (serious-condition (e)
       (incf *fail*) (format t "~&  FAIL ~A (~A)~%" ,name (type-of e)))))

;;; ---- scripted backend ----------------------------------------------

(defun assistant-resp (&key content tool-calls)
  "A parsed chat-completion response, shaped like the real API. CONTENT
   NIL becomes JSON null (the symbol NULL, as jzon would produce)."
  (let ((m (llm:ht "role" "assistant" "content" (or content 'null))))
    (when tool-calls (setf (gethash "tool_calls" m) tool-calls))
    (llm:ht "choices" (vector (llm:ht "message" m)))))

(defun bash-call (id cmd)
  (llm:ht "id" id "type" "function"
          "function" (llm:ht "name" "Bash"
                             "arguments" (format nil "{\"command\":~S}" cmd))))

(defun install-mock (responses)
  "Make DO-CHAT-WITH-RETRIES pop from RESPONSES; when exhausted, keep
   returning a null-content turn (so a bug can't loop forever)."
  (setf *script* (copy-list responses))
  (setf (symbol-function 'operandi.engine::do-chat-with-retries)
        (lambda (messages tools-vec &key verbose)
          (declare (ignore messages tools-vec verbose))
          (or (pop *script*) (assistant-resp :content 'null)))))

(defun final (&rest responses)
  "Run the loop against a scripted response list; return its final text."
  (install-mock responses)
  (values (eng:run "do the task" :verbose nil)))

;;; ---- cases ---------------------------------------------------------

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&operandi engine empty-turn oracle~%")

  ;; A normal single answer passes straight through.
  (check "normal answer returns verbatim"
    (string= "HELLO" (final (assistant-resp :content "HELLO"))))

  ;; A null-content final turn must NOT leak as the answer.
  (check "a single null turn never returns \"NULL\"/empty"
    (let ((r (final (assistant-resp :content 'null))))
      (and (stringp r) (plusp (length r))
           (not (string-equal r "NULL")) (not (string= r "")))))

  ;; Stall then recover: two empty turns get nudged, then a real answer.
  (check "nudges past empty turns, then returns the real answer"
    (string= "DONE"
             (final (assistant-resp :content 'null)
                    (assistant-resp :content 'null)
                    (assistant-resp :content "DONE"))))

  ;; Never-recovers: after the nudge budget, salvage the last real content
  ;; (here, the output of the one tool call it did make).
  (check "salvages the last tool result when the model only stalls"
    (let ((r (final (assistant-resp :tool-calls (vector (bash-call "c1" "echo SALVAGEME")))
                    (assistant-resp :content 'null)
                    (assistant-resp :content 'null)
                    (assistant-resp :content 'null)
                    (assistant-resp :content 'null))))
      (and (stringp r) (search "SALVAGEME" r))))

  ;; The empty counter resets on a productive turn — a null between two
  ;; tool calls shouldn't count toward the give-up budget.
  (check "empty counter resets after a tool call"
    (string= "FINISHED"
             (final (assistant-resp :tool-calls (vector (bash-call "c1" "echo one")))
                    (assistant-resp :content 'null)
                    (assistant-resp :tool-calls (vector (bash-call "c2" "echo two")))
                    (assistant-resp :content 'null)
                    (assistant-resp :content "FINISHED"))))

  ;; run returns a usage struct (calls/tokens/cost) as its 4th value.
  (check "run returns a usage struct as its 4th value"
    (progn
      (install-mock (list (assistant-resp :content "HI")))
      (multiple-value-bind (text history iters usage) (eng:run "task" :verbose nil)
        (declare (ignore text history iters))
        (and (typep usage 'llm:usage) (>= (llm:usage-calls usage) 1)))))

  ;; msg-text normalizer: null symbol / blank -> NIL; real string kept.
  (check "msg-text normalizes null and blank to NIL"
    (and (null (operandi.engine::msg-text (llm:ht "content" 'null)))
         (null (operandi.engine::msg-text (llm:ht "content" "   ")))
         (string= "x" (operandi.engine::msg-text (llm:ht "content" "x")))))

  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
