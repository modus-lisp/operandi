;;; src/store.lisp  (operandi.store)
;;;
;;; Thin cl-sqlite wrapper for operandi's own persistent state:
;;; idempotent schema init plus convenience macros for transactions
;;; and prepared statements. The only table it owns is
;;; AGENT_TOOL_CALLS — a queryable log of every tool invocation the
;;; agent makes (written by the default post-tool hook).
;;;
;;; A host application that wants richer domain tables keeps its own
;;; store and reaches it from the Eval tool; operandi.store is the
;;; agent's audit log, nothing more.
;;;
;;; Schema is versioned via PRAGMA user_version.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  ;; #+QUICKLISP: the .asd already loads these; this is for loading the file by hand.  Guarded
  ;; because READING `ql:' is an error in an image without Quicklisp, before anything runs.
  #+quicklisp (ql:quickload '(:bordeaux-threads) :silent t))

(defpackage #:operandi.store
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export #:*db* #:*default-path* #:*db-lock*
           #:open-store #:close-store
           #:with-tx #:with-db-lock
           #:exec #:exec-non-query #:select-rows #:select-single
           #:current-schema-version
           #:init-schema #:*exec-via* #:store-available-p))

(in-package #:operandi.store)

;;; SQLITE IS NOT A DEPENDENCY.  The store is an audit log, and cl-sqlite is a C library behind an FFI (or, on
;;; modus, a whole SQLite in Lisp) -- a lot to carry for a log.  It is reached by name: loaded on the first
;;; OPEN-STORE where ASDF can find "sqlite" (the desktop), and where it cannot (a phone) OPEN-STORE says so,
;;; *DB* stays NIL, and the tool-call hook, which ignores errors, logs nothing.

(defun store-available-p ()
  "True when a SQLite binding is loaded, or ASDF can find one to load."
  (or (find-package "SQLITE") (and (asdf:find-system "sqlite" nil) t)))

(defun %sq (name)
  "cl-sqlite's function NAME, loading \"sqlite\" first if it is findable and not loaded."
  (unless (find-package "SQLITE")
    (if (asdf:find-system "sqlite" nil)
        (asdf:load-system "sqlite")
        (error "operandi's store needs SQLite (the \"sqlite\" system), which is not in this image")))
  (symbol-function (find-symbol name "SQLITE")))

(defun %sqlite-error-p (e)
  (let ((c (and (find-package "SQLITE") (find-symbol "SQLITE-ERROR" "SQLITE"))))
    (and c (typep e c))))

(defvar *db* nil "Connection to the live SQLite handle.")

(defvar *db-lock* (bt:make-recursive-lock "operandi-store")
  "Serializes every access to *DB*. cl-sqlite's single connection and its
   per-connection prepared-statement cache are NOT safe for concurrent use
   across threads — and parallel subagents (Fan) log tool calls and run
   Eval queries at the same time. Every store op below holds this lock.
   Recursive so with-tx -> exec nests in one thread without deadlocking.")

(defmacro with-db-lock (&body body)
  "Run BODY holding *DB-LOCK*. Use to make a multi-statement sequence
   atomic against other threads (the store ops already lock individually)."
  `(bt:with-recursive-lock-held (*db-lock*) ,@body))

(defparameter *default-path*
  (merge-pathnames ".operandi/operandi.db" (user-homedir-pathname))
  "Durable home for operandi's agent tool-call log. A host application
   that binds this elsewhere (or opens with an explicit PATH) can point
   the log at its own store.")

(defun open-store (&optional (path *default-path*))
  "Open (or create) the store at PATH. Sets *DB* and returns the
   connection."
  (when *db* (funcall (%sq "DISCONNECT") *db*))
  (ensure-directories-exist path)
  (setf *db* (funcall (%sq "CONNECT") (namestring path)))
  ;; Wait for a lock instead of failing at once. Without this, operandi
  ;; processes STARTING TOGETHER (operandi_fan, a campaign driver, an
  ;; xargs -P) collide on the WAL switch below, and SQLite's default -- no
  ;; busy handler -- turns the collision into an immediate SQLITE_BUSY that
  ;; kills the process before its first turn.
  (funcall (%sq "EXECUTE-NON-QUERY") *db* "PRAGMA busy_timeout = 30000")
  ;; WAL mode: better concurrency and safer crashes for a long-running
  ;; reader+writer like the replay loop. Switching to it needs a moment of
  ;; exclusive access, which a busy timeout does not always cover (the
  ;; switch can return BUSY without invoking the handler), so retry.
  (loop for attempt from 1
        do (handler-case (progn (funcall (%sq "EXECUTE-NON-QUERY") *db* "PRAGMA journal_mode = WAL") (return))
             (error (e)
               (when (or (not (%sqlite-error-p e)) (>= attempt 50)) (error e))
               (sleep (+ 0.05 (random 0.2))))))
  (funcall (%sq "EXECUTE-NON-QUERY") *db* "PRAGMA synchronous = NORMAL")
  (funcall (%sq "EXECUTE-NON-QUERY") *db* "PRAGMA foreign_keys = ON")
  (init-schema)
  *db*)

(defun close-store ()
  (when *db* (funcall (%sq "DISCONNECT") *db*) (setf *db* nil)))

(defmacro with-tx (&body body)
  "Run BODY inside a transaction (rollback on error), holding the store
   lock for the whole transaction so no other thread interleaves."
  `(with-db-lock
     (funcall (%sq "EXECUTE-NON-QUERY") *db* "BEGIN")
     (handler-case
         (multiple-value-prog1 (progn ,@body)
           (funcall (%sq "EXECUTE-NON-QUERY") *db* "COMMIT"))
       (error (e)
         (funcall (%sq "EXECUTE-NON-QUERY") *db* "ROLLBACK")
         (error e)))))

(defvar *exec-via* nil
  "When non-NIL, a function (SQL PARAMS) that EXEC-NON-QUERY hands its write
   to instead of touching *DB*.  An actor that does not own the connection
   binds it to send the write to the one that does (see operandi.actor): on
   modus an actor may not mutate another's objects, and a connection is one.")

(defun exec-non-query (sql &rest params)
  "Execute SQL with PARAMS, return nothing meaningful."
  (if *exec-via*
      (progn (funcall *exec-via* sql params) nil)
      (with-db-lock (apply (%sq "EXECUTE-NON-QUERY") *db* sql params))))

(defun exec (sql &rest params)
  "Alias for exec-non-query."
  (apply #'exec-non-query sql params))

(defun select-rows (sql &rest params)
  "Run SELECT SQL with PARAMS, return list of rows (each a list of values)."
  (with-db-lock (apply (%sq "EXECUTE-TO-LIST") *db* sql params)))

(defun select-single (sql &rest params)
  "Single-value SELECT (e.g. SELECT COUNT(*) FROM …)."
  (with-db-lock (apply (%sq "EXECUTE-SINGLE") *db* sql params)))

;;; ----------------------- schema versioning -----------------------

(defun current-schema-version ()
  (with-db-lock (funcall (%sq "EXECUTE-SINGLE") *db* "PRAGMA user_version")))

(defparameter *schema-v1*
  ;; Operandi-agent tool-call log. Every Tool/Eval/Bash/etc invocation
  ;; from any operandi run goes here via the post-invoke hook. Lets us
  ;; analyze which tools the agent uses on which tasks, spot
  ;; thrashing patterns, and audit any concrete actions taken. This is
  ;; the ONLY table operandi's own store owns — a host application that
  ;; wants richer domain tables keeps its own store and reaches it from
  ;; the Eval tool.
  '("CREATE TABLE IF NOT EXISTS agent_tool_calls (
       id           INTEGER PRIMARY KEY AUTOINCREMENT,
       run_id       TEXT,                    -- groups calls in one agent run
       tool_name    TEXT NOT NULL,
       args_json    TEXT,
       result_text  TEXT,                    -- truncated to ~4KB
       error_text   TEXT,
       duration_ms  INTEGER,
       at           INTEGER NOT NULL
     )"
    "CREATE INDEX IF NOT EXISTS agent_tool_calls_run ON agent_tool_calls (run_id, at)"
    "CREATE INDEX IF NOT EXISTS agent_tool_calls_tool ON agent_tool_calls (tool_name, at)"))

(defun column-exists-p (table column)
  (some (lambda (r) (string= (second r) column))
        (funcall (%sq "EXECUTE-TO-LIST") *db* (format nil "PRAGMA table_info(~A)" table))))

(defun ensure-column (table column type)
  "ALTER TABLE … ADD COLUMN if not already present. Idempotent."
  (unless (column-exists-p table column)
    (funcall (%sq "EXECUTE-NON-QUERY") *db*
                         (format nil "ALTER TABLE ~A ADD COLUMN ~A ~A"
                                 table column type))))

(defun init-schema ()
  "Apply the schema to *DB*. Idempotent — CREATE … IF NOT EXISTS."
  (dolist (stmt *schema-v1*) (exec-non-query stmt))
  (let ((cur (current-schema-version)))
    (when (zerop cur)
      (exec-non-query "PRAGMA user_version = 1"))))
