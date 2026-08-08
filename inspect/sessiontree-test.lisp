;;; inspect/sessiontree-test.lisp
;;;
;;; Oracle for operandi.sessiontree — the append-only entry-tree with
;;; derive-state-by-replay + JSONL persistence. Deterministic, no network.
;;; Proves: append/leaf/branch-path; live state (model/tools/messages) is DERIVED
;;; from the branch and can't desync; branching (edit-a-past-turn forks a sibling);
;;; non-destructive compaction (navigate back to un-compact); a self-improvement
;;; critique as a branch entry; and crash-safe JSONL round-trip (incl. a torn line).
;;;
;;; Exit 0 iff all checks pass; joins fitness as a swarm/fitness oracle.

(require :asdf)
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:sessiontree-test
  (:use #:cl) (:local-nicknames (#:st #:operandi.sessiontree)))
(in-package #:sessiontree-test)

(defvar *fails* 0)
(defmacro check (name form)
  `(handler-case (if ,form (format t "  ok   ~A~%" ,name)
                     (progn (incf *fails*) (format t "  FAIL ~A~%" ,name)))
     (error (e) (incf *fails*) (format t "  ERR  ~A: ~A~%" ,name e))))

;; isolate all writes to a scratch dir
(setf (symbol-value (find-symbol "*TREES-DIR*" "OPERANDI.SESSIONTREE"))
      (namestring (merge-pathnames "operandi-sessiontree-test/" (uiop:temporary-directory))))
(ignore-errors (uiop:delete-directory-tree
                (pathname (symbol-value (find-symbol "*TREES-DIR*" "OPERANDI.SESSIONTREE")))
                :validate t :if-does-not-exist :ignore))

(defun contents (msgs) (mapcar (lambda (m) (gethash "content" m)) msgs))

(format t "~&== append builds a linear branch; leaf tracks the tip ==~%")
(let ((tr (st:make-tree :id "lin")))
  (check "fresh tree is empty"        (and (zerop (st:entries-count tr)) (null (st:leaf tr))))
  (st:add-message tr "user" "hello")
  (let ((a (st:leaf tr)))
    (st:add-message tr "assistant" "hi")
    (check "two entries, leaf moved"  (and (= 2 (st:entries-count tr)) (not (equal a (st:leaf tr)))))
    (check "branch-path is root->leaf order"
           (equal '("hello" "hi") (contents (st:messages tr))))
    (check "second entry's parent is the first"
           (equal a (st:parent-id (st:entry tr (st:leaf tr)))))))

(format t "~&== live state (model) is DERIVED from the branch, never a mutable field ==~%")
(let ((tr (st:make-tree :id "cfg")))
  (st:set-model tr "model-a")
  (st:add-message tr "user" "q1")
  (let ((at-a (st:leaf tr)))
    (st:set-model tr "model-b")
    (st:add-message tr "user" "q2")
    (check "current model = last model entry on branch" (equal "model-b" (st:current-model tr)))
    (check "navigating before the model change re-derives the OLD model"
           (equal "model-a" (st:current-model tr at-a)))
    (check "messages at the earlier leaf exclude later turns"
           (equal '("q1") (contents (st:messages tr at-a))))))

(format t "~&== branching: edit a past turn -> a sibling branch (no mutation) ==~%")
(let ((tr (st:make-tree :id "branch")))
  (st:add-message tr "user" "root")
  (let ((root (st:leaf tr)))
    (st:add-message tr "assistant" "answer-1")
    (st:add-message tr "user" "followup")
    (let ((tip1 (st:leaf tr)))
      (st:goto! tr root)                                  ; rewind to just after root
      (st:add-message tr "assistant" "answer-2")          ; fork
      (let ((tip2 (st:leaf tr)))
        (check "fork's parent is the shared root"
               (equal root (st:parent-id (st:entry tr tip2))))
        (check "branch 2 = root + answer-2"
               (equal '("root" "answer-2") (contents (st:messages tr tip2))))
        (check "branch 1 is intact and different"
               (equal '("root" "answer-1" "followup") (contents (st:messages tr tip1))))
        (check "the original entries were never mutated (4 total)"
               (= 4 (st:entries-count tr)))))))

(format t "~&== compaction is non-destructive; navigate back to un-compact ==~%")
(let ((tr (st:make-tree :id "compact")))
  (st:add-message tr "user" "old-1")
  (st:add-message tr "assistant" "old-2")
  (let ((before (st:leaf tr)))
    (st:compact! tr "the user asked X; I answered Y")
    (st:add-message tr "user" "new-1")
    (let ((msgs (st:messages tr)))
      (check "after compaction: summary first, then post-compaction msgs"
             (and (= 2 (length msgs))
                  (search "context summarized" (gethash "content" (first msgs)))
                  (equal "new-1" (gethash "content" (second msgs)))))
      (check "navigating to before the compaction restores full history"
             (equal '("old-1" "old-2") (contents (st:messages tr before)))))))

(format t "~&== a self-improvement critique rides the branch as a custom entry ==~%")
(let ((tr (st:make-tree :id "review")))
  (st:add-message tr "user" "do the thing")
  (st:add-message tr "assistant" "done")
  (st:add-custom tr "critique" "next time, verify before claiming done")
  (check "custom entry is on the branch"
         (find "custom" (mapcar #'st:etype (st:branch-path tr)) :test #'string=))
  (check "custom entry is NOT a chat message (messages unchanged)"
         (equal '("do the thing" "done") (contents (st:messages tr)))))

(format t "~&== JSONL persistence: incremental append round-trips ==~%")
(let ((tr (st:make-tree :id "persist")))
  (st:set-model tr "m1" :persist t)
  (st:add-message tr "user" "hi" :persist t)
  (st:add-message tr "assistant" "yo" :persist t)
  (let ((leaf (st:leaf tr))
        (re (st:load-tree "persist")))
    (check "reloaded entry count matches"  (= (st:entries-count tr) (st:entries-count re)))
    (check "reloaded leaf matches"         (equal leaf (st:leaf re)))
    (check "reloaded messages match"       (equal '("hi" "yo") (contents (st:messages re))))
    (check "reloaded derived model matches" (equal "m1" (st:current-model re)))
    (check "reloaded next append continues the seq (no id collision)"
           (progn (st:add-message re "user" "again")
                  (= (1+ (st:entries-count tr)) (st:entries-count re))))))

(format t "~&== JSONL persistence: navigation (leaf record) survives reload ==~%")
(let ((tr (st:make-tree :id "nav")))
  (st:add-message tr "user" "a" :persist t)
  (let ((a (st:leaf tr)))
    (st:add-message tr "assistant" "b" :persist t)
    (st:goto! tr a :persist t)                            ; journal the leaf move
    (let ((re (st:load-tree "nav")))
      (check "reloaded leaf reflects the journaled navigation" (equal a (st:leaf re)))
      (check "reloaded branch at that leaf" (equal '("a") (contents (st:messages re)))))))

(format t "~&== crash safety: a torn trailing line is skipped on load ==~%")
(let ((tr (st:make-tree :id "torn")))
  (st:add-message tr "user" "intact" :persist t)
  (with-open-file (s (st:tree-path "torn") :direction :output :if-exists :append
                     :if-does-not-exist :create)
    (write-string "{\"id\":\"e99999\",\"type\":\"message\",\"payl" s))  ; truncated JSON
  (let ((re (st:load-tree "torn")))
    (check "torn line ignored; good entry survives"
           (and (= 1 (st:entries-count re)) (equal '("intact") (contents (st:messages re)))))))

(format t "~&~%sessiontree-test: ~A failure~:P~%" *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
