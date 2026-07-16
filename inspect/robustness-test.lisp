;;; inspect/robustness-test.lisp
;;;
;;; Oracle for the tool sandbox's robustness guarantees. Every case
;;; feeds a tool an input that — before the hardening — would hang the
;;; agent loop forever or crash the whole SBCL image past the per-tool
;;; handler-case. The test passing at all is itself the proof: if a case
;;; regressed, this process would freeze or die instead of reaching the
;;; assertions.
;;;
;;;   sbcl --non-interactive --load inspect/robustness-test.lisp
;;;
;;; Exits 0 if every case holds, 1 otherwise — usable as a swarm oracle.

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.robustness-test
  (:use #:cl)
  (:local-nicknames (#:tools #:operandi.tools)
                    (#:llm   #:operandi.llm))
  (:export #:run))
(in-package #:operandi.robustness-test)

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro check (name &body body)
  "BODY returns non-nil to pass. Bounds each case at 20s of wall clock so
   a genuine hang (regression) fails loudly instead of freezing the run."
  `(handler-case
       (if (sb-ext:with-timeout 20 (progn ,@body))
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A (assertion false)~%" ,name)))
     (sb-ext:timeout ()
       (incf *fail*)
       (format t "~&  FAIL ~A (case itself hung >20s — the guard did not fire)~%" ,name))
     (serious-condition (e)
       (incf *fail*)
       (format t "~&  FAIL ~A (escaped as ~A)~%" ,name (type-of e)))))

(defun eval-tool (form)
  (tools:invoke-tool "Eval" (llm:ht "form" form)))
(defun bash-tool (cmd)
  (tools:invoke-tool "Bash" (llm:ht "command" cmd)))

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&operandi tool-sandbox robustness oracle~%")

  ;; 1. Deep recursion → control-stack exhaustion (storage-condition).
  ;;    Must come back as a caught error string, not crash the image.
  (check "Eval: stack overflow is caught"
    (search "exhausted" (eval-tool "(labels ((f (x) (+ 1 (f x)))) (f 1))")))

  ;; 2. Infinite loop in an Eval'd form → must abort on the timeout.
  (check "Eval: infinite loop times out"
    (let ((tools:*eval-timeout* 2)) ; keep the test quick
      (search "longer than" (eval-tool "(loop)"))))

  ;; 3. A hanging shell command → coreutils timeout must kill it.
  (check "Bash: hanging command is killed"
    (let ((tools:*bash-timeout* 1))
      (search "killed" (bash-tool "sleep 30"))))

  ;; 4. Read on an endless non-regular file must NOT OOM the image.
  ;;    (Before the fix this was a fatal, uncatchable "GC invariant lost".)
  (check "Read: /dev/zero is refused, not OOM"
    (search "not a regular file"
            (tools:invoke-tool "Read" (llm:ht "path" "/dev/zero"))))
  (check "Read: a directory is refused"
    (search "not a regular file"
            (tools:invoke-tool "Read" (llm:ht "path" "/tmp"))))

  ;; 5. Grep / Glob must not let an arg run a shell command (injection).
  (let ((canary (merge-pathnames "operandi-inj-canary"
                                 (uiop:temporary-directory))))
    (check "Grep: no shell injection via pattern"
      (ignore-errors (delete-file canary))
      (tools:invoke-tool "Grep"
        (llm:ht "pattern" (format nil "x$(touch ~A)" (namestring canary)) "path" "."))
      (not (probe-file canary)))
    (check "Glob: no shell injection via pattern"
      (ignore-errors (delete-file canary))
      (tools:invoke-tool "Glob"
        (llm:ht "pattern" (format nil "*$(touch ~A)" (namestring canary))))
      (not (probe-file canary))))

  ;; Globstar + `..` explodes bash's path set into a freeze — must refuse.
  (check "Glob: globstar-with-.. is refused, not a freeze"
    (search "must be" (tools:invoke-tool "Glob"
                        (llm:ht "pattern" "**/../**/../../**"))))

  ;; 5b. Edit guards: empty OLD and oversized files are refused up front.
  (check "Edit: empty OLD is refused"
    (let ((f "/tmp/operandi-rt-edit.txt"))
      (with-open-file (s f :direction :output :if-exists :supersede)
        (write-string "abc" s))
      (search "must not be empty"
              (tools:invoke-tool "Edit" (llm:ht "path" f "old" "" "new" "z")))))
  (check "Edit: oversized file is refused, not read into memory"
    (let ((f "/tmp/operandi-rt-edit.txt")
          (tools:*edit-max-bytes* 4))
      (with-open-file (s f :direction :output :if-exists :supersede)
        (write-string "way more than four bytes" s))
      (search "too large" (tools:invoke-tool "Edit" (llm:ht "path" f "old" "way" "new" "z")))))

  ;; 5c. WebFetch: an unreachable URL comes back as an error, not a crash.
  (check "WebFetch: unreachable URL returns an error string"
    (let ((r (tools:invoke-tool "WebFetch" (llm:ht "url" "http://127.0.0.1:1/"))))
      (and (stringp r) (search "FETCH ERROR" r))))

  ;; 6. Happy paths still work (the guards didn't break normal use).
  (check "Eval: normal form still returns"
    (search "=> 3" (eval-tool "(+ 1 2)")))
  (check "Bash: normal command still returns"
    (search "hi" (bash-tool "echo hi")))
  (check "Read: a normal file still reads"
    (let ((f "/tmp/operandi-rt-normal.txt"))
      (with-open-file (s f :direction :output :if-exists :supersede)
        (write-line "line one" s) (write-line "line two" s))
      (search "line two" (tools:invoke-tool "Read" (llm:ht "path" f)))))
  (check "Grep: a normal search still works"
    (let ((r (tools:invoke-tool "Grep" (llm:ht "pattern" "defun" "path" "."))))
      (and (stringp r) (not (search "must be" r)))))
  (check "Edit: a normal edit still works"
    (let ((f "/tmp/operandi-rt-edit2.txt"))
      (with-open-file (s f :direction :output :if-exists :supersede)
        (write-string "the quick brown fox" s))
      (tools:invoke-tool "Edit" (llm:ht "path" f "old" "quick" "new" "slow"))
      (search "slow" (uiop:read-file-string f))))

  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(let ((ok (run)))
  (when (member :non-interactive-exit sb-ext:*posix-argv* :test #'string=) nil)
  (sb-ext:exit :code (if ok 0 1)))
