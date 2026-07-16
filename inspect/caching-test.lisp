;;; inspect/caching-test.lisp
;;;
;;; Oracle for prompt-cache support. Two verifiable parts (no network):
;;;   1. fold-usage captures prefix-cache hits from the provider's usage
;;;      block (OpenAI/OpenRouter prompt_tokens_details.cached_tokens, and
;;;      Anthropic cache_read_input_tokens).
;;;   2. the Anthropic cache_control request SHAPE — gated on the model,
;;;      non-destructive, correct block structure.
;;;
;;; NOT covered (can't be, here): live cache HITS against a real Claude
;;; model — the current OpenRouter key has no Claude access. That the
;;; providers we DO use already auto-cache (~74% hit) is shown by the eval
;;; suite's cache line, not this file.
;;;
;;;   sbcl --non-interactive --load inspect/caching-test.lisp   (0/1)

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.caching-test
  (:use #:cl)
  (:local-nicknames (#:eng #:operandi.engine) (#:llm #:operandi.llm)))
(in-package #:operandi.caching-test)

(defvar *pass* 0) (defvar *fail* 0)
(defmacro check (name &body body)
  `(handler-case
       (if (progn ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A~%" ,name)))
     (serious-condition (e)
       (incf *fail*) (format t "~&  FAIL ~A (~A)~%" ,name (type-of e)))))

(defun parsed-with-usage (usage-ht)
  (llm:ht "choices" (vector (llm:ht "message" (llm:ht "role" "assistant" "content" "x")))
          "usage" usage-ht))

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&operandi prompt-cache oracle~%")

  ;; 1. cache-hit capture — OpenAI/OpenRouter shape.
  (check "fold-usage captures cached_tokens (prompt_tokens_details)"
    (let ((u (llm:make-usage)))
      (llm:fold-usage u (parsed-with-usage
                         (llm:ht "prompt_tokens" 100 "completion_tokens" 10
                                 "prompt_tokens_details" (llm:ht "cached_tokens" 80))))
      (and (= (llm:usage-prompt-tokens u) 100) (= (llm:usage-cached-tokens u) 80))))

  ;; 1b. cache-hit capture — Anthropic shape.
  (check "fold-usage captures cache_read_input_tokens (Anthropic)"
    (let ((u (llm:make-usage)))
      (llm:fold-usage u (parsed-with-usage
                         (llm:ht "prompt_tokens" 50 "completion_tokens" 5
                                 "cache_read_input_tokens" 40)))
      (= (llm:usage-cached-tokens u) 40)))

  ;; 1c. no cache info -> 0, and usage-incf carries cached across roll-up.
  (check "cached tokens roll up through usage-incf"
    (let ((a (llm:make-usage :cached-tokens 30)) (b (llm:make-usage :cached-tokens 12)))
      (llm:usage-incf a b)
      (= (llm:usage-cached-tokens a) 42)))

  ;; 2. model gating.
  (check "anthropic-model-p classifies models"
    (and (eng::anthropic-model-p "anthropic/claude-sonnet-4-5")
         (eng::anthropic-model-p "some/claude-thing")
         (not (eng::anthropic-model-p "deepseek/deepseek-v4-flash"))
         (not (eng::anthropic-model-p nil))))

  ;; 2b. the cache_control transform shape.
  (check "cache-control-system marks the system block, leaves the rest"
    (let* ((msgs (vector (llm:ht "role" "system" "content" "SYS")
                         (llm:ht "role" "user" "content" "hi")))
           (out (eng::cache-control-system msgs))
           (sys (aref out 0))
           (blocks (gethash "content" sys))
           (blk (and (vectorp blocks) (plusp (length blocks)) (aref blocks 0))))
      (and (vectorp blocks)
           (equal (gethash "type" blk) "text")
           (equal (gethash "text" blk) "SYS")
           (equal (gethash "type" (gethash "cache_control" blk)) "ephemeral")
           ;; second message untouched (still a plain string)
           (stringp (gethash "content" (aref out 1)))
           ;; original vector not mutated
           (stringp (gethash "content" (aref msgs 0))))))

  ;; 2c. no-op when the first message isn't a plain-text system message.
  (check "cache-control-system is a no-op without a leading system message"
    (let ((msgs (vector (llm:ht "role" "user" "content" "hi"))))
      (stringp (gethash "content" (aref (eng::cache-control-system msgs) 0)))))

  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
