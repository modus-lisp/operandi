;;; src/hooks.lisp
;;;
;;; Pre/post-tool hooks for operandi. A hook is a function called at
;;; a defined point in the tool-invocation lifecycle. Multiple hooks
;;; can be registered; they run in order.
;;;
;;; Pre-tool hook signature:
;;;   (fn TOOL-NAME ARGS-HASH) -> nothing meaningful
;;;
;;; Post-tool hook signature:
;;;   (fn TOOL-NAME ARGS-HASH RESULT-STRING ERROR-OR-NIL DURATION-MS) -> nothing
;;;
;;; The default post-tool hook logs every invocation to the
;;; agent_tool_calls SQLite table — gives us a queryable history
;;; of what operandi did, when, with what arguments, and what came
;;; back. Easy to disable by clearing *POST-TOOL-HOOKS*.
;;;
;;; Run-id correlation: the engine binds *CURRENT-RUN-ID* at the
;;; start of each RUN call. Hooks read it and tag their log rows so
;;; calls within a single agent run are grouped.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (ql:quickload '(:com.inuoe.jzon) :silent t))

(defpackage #:operandi.hooks
  (:use #:cl)
  (:local-nicknames (#:jzon  #:com.inuoe.jzon)
                    (#:store #:operandi.store))
  (:export #:*pre-tool-hooks*
           #:*post-tool-hooks*
           #:*current-run-id*
           #:with-run-id
           #:run-pre-hooks
           #:run-post-hooks
           #:log-tool-call-to-sqlite))

(in-package #:operandi.hooks)

(defvar *pre-tool-hooks* '()
  "Functions called before every tool invocation. Each is
   (fn tool-name args-hash).")

(defvar *post-tool-hooks* '()
  "Functions called after every tool invocation. Each is
   (fn tool-name args-hash result-string error-or-nil duration-ms).")

(defvar *current-run-id* nil
  "Bound by the engine for the duration of one RUN call so hooks
   can correlate tool calls back to the agent invocation.")

(defmacro with-run-id (id &body body)
  `(let ((*current-run-id* ,id)) ,@body))

(defun run-pre-hooks (tool-name args)
  (dolist (h *pre-tool-hooks*)
    (handler-case (funcall h tool-name args)
      (error (e) (format *error-output* "~&pre-hook err: ~A~%" e)))))

(defun run-post-hooks (tool-name args result error duration-ms)
  (dolist (h *post-tool-hooks*)
    (handler-case (funcall h tool-name args result error duration-ms)
      (error (e) (format *error-output* "~&post-hook err: ~A~%" e)))))

;;; ----------------------- default sqlite logger -------------------

(defun safe-stringify (val &key (max 4000))
  (let ((s (handler-case
               (etypecase val
                 (string val)
                 (hash-table (jzon:stringify val))
                 (null "")
                 (t (princ-to-string val)))
             (error () (princ-to-string val)))))
    (if (> (length s) max)
        (concatenate 'string (subseq s 0 max) "...[truncated]")
        s)))

(defun log-tool-call-to-sqlite (tool-name args result error duration-ms)
  "Default post-tool hook. Writes one row to agent_tool_calls."
  (handler-case
      (store:exec
       "INSERT INTO agent_tool_calls
          (run_id, tool_name, args_json, result_text, error_text,
           duration_ms, at)
        VALUES (?, ?, ?, ?, ?, ?, ?)"
       *current-run-id*
       tool-name
       (safe-stringify args :max 8000)
       (safe-stringify result :max 8000)
       (and error (princ-to-string error))
       duration-ms
       (- (get-universal-time) 2208988800))
    (error () nil)))

;; Register the SQLite logger as the default post-hook. Anyone who
;; wants to disable it can (setf *POST-TOOL-HOOKS* nil) at runtime.
(pushnew #'log-tool-call-to-sqlite *post-tool-hooks*)
