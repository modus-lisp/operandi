;;; src/session.lisp  (operandi.session)
;;;
;;; Conversation-session state + persistence, shared by every front-end (the
;;; TUI and the ACP server). A session is a plain HASH-TABLE, NOT a defstruct —
;;; on purpose: operandi can reload/recompile itself in its own running image
;;; (the Eval tool, self-improvement), and a live defstruct instance turns "not
;;; of type SESSION" the instant its defstruct is re-evaluated. A hash-table has
;;; no such layout coupling; usage totals are kept as raw numbers (a fresh
;;; llm:usage is built on demand for display) for the same reason.
;;;
;;; Each session persists to ~/.operandi/sessions/<id>.{md,json}, rewritten after
;;; every turn (crash-safe). The .json holds the RAW eng:run message history, so
;;; a session can be resumed by re-sending the exact context verbatim.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (ql:quickload '(:com.inuoe.jzon :uiop) :silent t))

(defpackage #:operandi.session
  (:use #:cl)
  (:local-nicknames (#:llm  #:operandi.llm)
                    (#:jzon #:com.inuoe.jzon))
  (:export #:*sessions-dir* #:make-session #:reset-session! #:session-id
           #:session-history #:session-turns #:session-usage #:session-add-usage
           #:persist-session #:render-session #:session->json #:session-json-path
           #:list-sessions #:resume-session! #:model-label))

(in-package #:operandi.session)

(defparameter *sessions-dir*
  (namestring (merge-pathnames ".operandi/sessions/" (user-homedir-pathname)))
  "Where each session's transcript (.md) + machine record (.json) is written,
   updated after every turn (so a crash never loses it).")

(defun model-label ()
  (case llm:*llm-backend*
    (:openrouter (or llm:*llm-model* "openrouter"))
    (t "llama.cpp")))

(defun session-id ()
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time))
    (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D" y mo d h mi s)))

(defun reset-session! (s)
  "(Re)initialise session hash-table S with a fresh id and zeroed totals."
  (setf (gethash :id s) (session-id)
        (gethash :history s) nil
        (gethash :turns s) 0
        (gethash :cost s) 0d0
        (gethash :prompt-tok s) 0
        (gethash :completion-tok s) 0
        (gethash :cached-tok s) 0
        (gethash :calls s) 0)
  s)

(defun make-session () (reset-session! (make-hash-table :test 'eq)))

(defun session-history (s) (gethash :history s))
(defun (setf session-history) (v s) (setf (gethash :history s) v))
(defun session-turns (s) (gethash :turns s))

(defun session-add-usage (s u)
  "Fold one turn's usage (an llm:usage) into the session's raw totals."
  (when u
    (incf (gethash :cost s)           (llm:usage-cost-usd u))
    (incf (gethash :prompt-tok s)     (llm:usage-prompt-tokens u))
    (incf (gethash :completion-tok s) (llm:usage-completion-tokens u))
    (incf (gethash :cached-tok s)     (llm:usage-cached-tokens u))
    (incf (gethash :calls s)          (llm:usage-calls u))))

(defun session-usage (s)
  "A transient llm:usage built from the session's raw totals — freshly
   constructed, so it always has the current struct layout."
  (llm:make-usage :cost-usd (gethash :cost s)
                  :prompt-tokens (gethash :prompt-tok s)
                  :completion-tokens (gethash :completion-tok s)
                  :cached-tokens (gethash :cached-tok s)
                  :calls (gethash :calls s)))

;;; --- persistence (~/.operandi/sessions/<id>.{md,json}) ---

(defun %clip (s max)
  (let ((s (if (stringp s) s (princ-to-string (or s "")))))
    (if (> (length s) max)
        (format nil "~A~%…[~:D more chars]" (subseq s 0 max) (- (length s) max))
        s)))

(defun render-session (s)
  "A readable markdown transcript. System prompt omitted; tool results clipped
   (the raw record lives in the SQLite tool-call log)."
  (with-output-to-string (o)
    (format o "# operandi session ~A~%~%- model: ~A~%- turns: ~A~%- ~A~%~%---~%~%"
            (gethash :id s) (model-label) (gethash :turns s)
            (llm:usage-summary (session-usage s)))
    (dolist (m (gethash :history s))
      (let ((role (gethash "role" m)) (c (gethash "content" m))
            (tcs (gethash "tool_calls" m)))
        (unless (string= role "system")
          (format o "**~A:** ~A~%~%" role (%clip (or (and (stringp c) c) "") 6000))
          (when (and tcs (or (vectorp tcs) (listp tcs)))
            (map nil (lambda (tc)
                       (let ((fn (and (hash-table-p tc) (gethash "function" tc))))
                         (when (hash-table-p fn)
                           (format o "    → ~A(~A)~%" (gethash "name" fn)
                                   (%clip (gethash "arguments" fn) 400)))))
                 (if (listp tcs) (coerce tcs 'vector) tcs))
            (terpri o)))))))

(defun session-json-path (id)
  (merge-pathnames (format nil "~A.json" id) *sessions-dir*))

(defun session->json (s)
  "Serialize the session — id, model, totals, and the RAW message history."
  (jzon:stringify
   (llm:ht "id"      (gethash :id s)
           "model"   (model-label)
           "backend" (string-downcase (symbol-name llm:*llm-backend*))
           "turns"   (gethash :turns s)
           "usage"   (llm:ht "cost" (gethash :cost s)
                             "prompt" (gethash :prompt-tok s)
                             "completion" (gethash :completion-tok s)
                             "cached" (gethash :cached-tok s)
                             "calls" (gethash :calls s))
           "history" (coerce (gethash :history s) 'vector))
   :pretty t))

(defun persist-session (s)
  "Write the session's .md transcript + .json record. Best-effort — never lets
   an I/O error take down the caller."
  (handler-case
      (progn
        (ensure-directories-exist *sessions-dir*)
        (with-open-file (o (merge-pathnames (format nil "~A.md" (gethash :id s))
                                            *sessions-dir*)
                           :direction :output :if-exists :supersede
                           :if-does-not-exist :create :external-format :utf-8)
          (write-string (render-session s) o))
        (with-open-file (o (session-json-path (gethash :id s))
                           :direction :output :if-exists :supersede
                           :if-does-not-exist :create :external-format :utf-8)
          (write-string (session->json s) o))
        t)
    (error () nil)))

(defun list-sessions ()
  "Saved sessions, newest first: a list of (id turns first-user-prompt)."
  (let ((files (ignore-errors (directory (merge-pathnames "*.json" *sessions-dir*)))))
    (loop for f in (sort (copy-list files) #'> :key #'file-write-date)
          collect (handler-case
                      (let* ((d (jzon:parse (uiop:read-file-string f)))
                             (hist (coerce (or (gethash "history" d) #()) 'vector))
                             (first-user (loop for m across hist
                                               when (and (hash-table-p m)
                                                         (equal (gethash "role" m) "user"))
                                               return (gethash "content" m))))
                        (list (pathname-name f) (or (gethash "turns" d) 0) (or first-user "")))
                    (error () (list (pathname-name f) 0 "(unreadable)"))))))

(defun resume-session! (s target)
  "Load a saved session into hash-table S in place. TARGET is a session id
   string or :LATEST. Returns the id on success, NIL if not found/unreadable.
   The resumed session keeps its id, so continuing appends to the same files."
  (let ((id (if (eq target :latest)
                (let ((all (list-sessions))) (and all (first (first all))))
                target)))
    (when id
      (handler-case
          (let* ((path (session-json-path id))
                 (d (and (probe-file path) (jzon:parse (uiop:read-file-string path)))))
            (when (hash-table-p d)
              (let ((u (gethash "usage" d)) (h (gethash "history" d)))
                (flet ((u@ (k) (or (and (hash-table-p u) (gethash k u)) 0)))
                  (setf (gethash :id s) id
                        (gethash :history s) (if (vectorp h) (coerce h 'list) h)
                        (gethash :turns s) (or (gethash "turns" d) 0)
                        (gethash :cost s) (float (u@ "cost") 1d0)
                        (gethash :prompt-tok s) (u@ "prompt")
                        (gethash :completion-tok s) (u@ "completion")
                        (gethash :cached-tok s) (u@ "cached")
                        (gethash :calls s) (u@ "calls")))
                id)))
        (error () nil)))))
