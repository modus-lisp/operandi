;;; src/sessiontree.lisp  (operandi.sessiontree)
;;;
;;; A session as an append-only, immutable *entry tree* with a movable LEAF
;;; pointer — a git-like DAG, not a linear history. Adapted from earendil-works's
;;; pi (see docs/agent-survey.md, "session entry-tree"), which is the substrate
;;; that makes crash-safe resumption, branching ("edit a past turn and re-run"),
;;; and a self-improvement review-fork (its critique = a branch entry) all fall
;;; out of ONE primitive.
;;;
;;; The load-bearing rule: entries are IMMUTABLE and live state is never a mutated
;;; field — the model, active tools, and message list are DERIVED by folding the
;;; path from the leaf back to the root (DERIVE-STATE). So state can never desync
;;; from history, and resuming is just "reduce the branch again."
;;;
;;; Persistence is JSONL: one entry per line, append-only (crash-safe — a torn
;;; trailing line is skipped on load), human-readable, git-diffable. Navigation
;;; (moving the leaf without appending) is journaled as a {"type":"leaf"} record,
;;; so the current leaf survives a reload.
;;;
;;; Reload-safe like operandi.session: a tree is a plain HASH-TABLE, no defstruct,
;;; so recompiling this file in a live image never orphans a running session.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (funcall (read-from-string "ql:quickload") '(:com.inuoe.jzon :uiop) :silent t))

(defpackage #:operandi.sessiontree
  (:use #:cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:export #:*trees-dir* #:make-tree #:tree-id #:tree-path
           #:append-entry #:add-message #:set-model #:set-tools #:add-custom #:compact!
           #:entry #:leaf #:entries-count #:entries-in-order #:goto! #:branch-path
           #:etype #:payload #:parent-id #:entry-id
           #:derive-state #:messages #:current-model #:current-tools
           #:save-tree #:load-tree))
(in-package #:operandi.sessiontree)

(defparameter *trees-dir*
  (namestring (merge-pathnames ".operandi/trees/" (user-homedir-pathname)))
  "Where each session tree's append-only JSONL log is written.")

(defun unix-now () (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))
(defun gen-id ()
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time))
    (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D-~(~4,'0x~)"
            y mo d h mi s (mod (get-internal-real-time) #x10000))))

;;; ------------------------------- tree object -------------------------------
(defun make-tree (&key (id (gen-id)))
  "A fresh empty session tree (leaf = NIL)."
  (let ((tr (make-hash-table :test 'eq)))
    (setf (gethash :id tr) id
          (gethash :entries tr) (make-hash-table :test 'equal)   ; id -> entry ht
          (gethash :order tr) nil                                ; ids in creation order (reversed)
          (gethash :leaf tr) nil
          (gethash :seq tr) 0)
    tr))
(defun tree-id (tr) (gethash :id tr))
(defun leaf (tr) (gethash :leaf tr))
(defun entry (tr id) (and id (gethash id (gethash :entries tr))))
(defun entries-count (tr) (hash-table-count (gethash :entries tr)))
(defun entries-in-order (tr) (reverse (gethash :order tr)))
(defun new-id (tr) (format nil "e~5,'0d" (incf (gethash :seq tr))))

;;; entry accessors (an entry is a hash-table with string keys, for clean JSON)
(defun entry-id (e)  (gethash "id" e))
(defun parent-id (e) (gethash "parent" e))
(defun etype (e)     (gethash "type" e))
(defun payload (e)   (gethash "payload" e))

;;; ------------------------------- append (core) -----------------------------
(defun %entry (id parent type payload)
  (let ((e (make-hash-table :test 'equal)))
    (setf (gethash "id" e) id
          (gethash "parent" e) parent
          (gethash "type" e) (string-downcase (symbol-name type))
          (gethash "payload" e) payload
          (gethash "created" e) (unix-now))
    e))
(defun %intern (tr e)
  (setf (gethash (entry-id e) (gethash :entries tr)) e
        (gethash :leaf tr) (entry-id e))
  (push (entry-id e) (gethash :order tr))
  e)

(defun append-entry (tr type payload &key persist)
  "Append a new immutable TYPE entry as a child of the current leaf; move the
   leaf to it. Returns the new entry's id. With :PERSIST, append it to the JSONL."
  (let ((e (%intern tr (%entry (new-id tr) (gethash :leaf tr) type payload))))
    (when persist (%write-line tr e))
    (entry-id e)))

;;; typed conveniences -------------------------------------------------------
(defun add-message (tr role content &key tool-calls tool-call-id persist)
  "Append a chat message entry (the payload is a resend-ready message object)."
  (let ((m (make-hash-table :test 'equal)))
    (setf (gethash "role" m) role (gethash "content" m) content)
    (when tool-calls (setf (gethash "tool_calls" m) tool-calls))
    (when tool-call-id (setf (gethash "tool_call_id" m) tool-call-id))
    (append-entry tr :message m :persist persist)))
(defun set-model  (tr model &key persist) (append-entry tr :model  model :persist persist))
(defun set-tools  (tr names &key persist) (append-entry tr :tools  (coerce names 'vector) :persist persist))
(defun add-custom (tr tag value &key persist)
  "A freeform entry (e.g. a self-improvement review-fork's critique). Payload is
   {tag, value}; it rides the branch but is not a chat message."
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "tag" h) tag (gethash "value" h) value)
    (append-entry tr :custom h :persist persist)))
(defun compact! (tr summary-text &key persist)
  "Append a compaction entry: on this branch, all messages BEFORE it collapse to
   SUMMARY-TEXT; messages after it stay verbatim. Non-destructive — navigating to
   an entry before it un-compacts (the compaction isn't on that branch)."
  (append-entry tr :compaction summary-text :persist persist))

;;; ------------------------------- navigation --------------------------------
(defun goto! (tr id &key persist)
  "Move the leaf to an existing entry. The NEXT append there forks a sibling
   branch — this is the edit-a-past-turn-and-re-run primitive."
  (unless (entry tr id) (error "sessiontree: no such entry ~a" id))
  (setf (gethash :leaf tr) id)
  (when persist (%write-leaf tr id))
  id)

;;; --------------------- derive live state by folding the branch -------------
(defun branch-path (tr &optional (leaf (gethash :leaf tr)))
  "The active branch: entries from root down to LEAF, in order."
  (let ((out '()) (cur leaf))
    (loop while cur for e = (entry tr cur) while e do
      (push e out)
      (setf cur (parent-id e)))
    out))

(defun %summary-message (text)
  (let ((m (make-hash-table :test 'equal)))
    (setf (gethash "role" m) "user"
          (gethash "content" m) (format nil "[earlier context summarized]~%~a" text))
    m))

(defun derive-state (tr &optional (leaf (gethash :leaf tr)))
  "Fold the branch into (values MESSAGES MODEL TOOLS). MESSAGES is the resend-ready
   chat history (a compaction entry collapses everything before it to its summary);
   MODEL/TOOLS are the LAST such config entry seen on the branch — so live state is
   reconstructed, never a mutable field that can drift out of sync with history."
  (let ((msgs '()) (model nil) (tools nil))
    (dolist (e (branch-path tr leaf))
      (let ((ty (etype e)))
        (cond
          ((string= ty "message")    (push (payload e) msgs))
          ((string= ty "model")      (setf model (payload e)))
          ((string= ty "tools")      (setf tools (payload e)))
          ((string= ty "compaction") (setf msgs (list (%summary-message (payload e))))))))
    (values (nreverse msgs) model tools)))

(defun messages      (tr &optional (leaf (gethash :leaf tr))) (nth-value 0 (derive-state tr leaf)))
(defun current-model (tr &optional (leaf (gethash :leaf tr))) (nth-value 1 (derive-state tr leaf)))
(defun current-tools (tr &optional (leaf (gethash :leaf tr)))
  (let ((v (nth-value 2 (derive-state tr leaf)))) (and v (coerce v 'list))))

;;; ------------------------------- persistence (JSONL) -----------------------
(defun tree-path (id)
  (merge-pathnames (format nil "~a.jsonl" id) *trees-dir*))
(defun %append-line (tr obj)
  (ensure-directories-exist *trees-dir*)
  (with-open-file (s (tree-path (tree-id tr)) :direction :output
                     :if-exists :append :if-does-not-exist :create
                     :external-format :utf-8)
    (write-line (jzon:stringify obj) s)))
(defun %write-line (tr e) (%append-line tr e))
(defun %write-leaf (tr id)
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "type" h) "leaf" (gethash "target" h) id)
    (%append-line tr h)))

(defun save-tree (tr)
  "Rewrite the whole JSONL from scratch (entries in creation order + a trailing
   leaf marker). Use for a fresh dump/migration; normal operation appends
   incrementally via the :persist keyword."
  (ensure-directories-exist *trees-dir*)
  (with-open-file (s (tree-path (tree-id tr)) :direction :output
                     :if-exists :supersede :if-does-not-exist :create
                     :external-format :utf-8)
    (dolist (id (entries-in-order tr))
      (write-line (jzon:stringify (entry tr id)) s))
    (let ((h (make-hash-table :test 'equal)))
      (setf (gethash "type" h) "leaf" (gethash "target" h) (gethash :leaf tr))
      (write-line (jzon:stringify h) s)))
  tr)

(defun load-tree (id-or-path)
  "Rebuild a tree from its JSONL by replaying every line (append-only ⇒ replay is
   the recovery model). A real entry advances the leaf to itself; a leaf record
   overrides it; an unparseable trailing line (torn write) is skipped."
  (let* ((path (if (pathnamep id-or-path) id-or-path (tree-path id-or-path)))
         (tr (make-tree :id (pathname-name path))))
    (when (probe-file path)
      (with-open-file (s path :external-format :utf-8)
        (loop for line = (read-line s nil nil) while line
              for rec = (ignore-errors (jzon:parse line))
              when (hash-table-p rec) do
                (cond
                  ((string= (gethash "type" rec "") "leaf")
                   (setf (gethash :leaf tr) (gethash "target" rec)))
                  ((entry-id rec)
                   (setf (gethash (entry-id rec) (gethash :entries tr)) rec
                         (gethash :leaf tr) (entry-id rec))
                   (push (entry-id rec) (gethash :order tr))
                   (let ((n (ignore-errors (parse-integer (entry-id rec) :start 1))))
                     (when (and n (> n (gethash :seq tr))) (setf (gethash :seq tr) n))))))))
    tr))
