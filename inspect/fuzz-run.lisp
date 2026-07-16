;;; inspect/fuzz-run.lisp
;;;
;;; Batch fuzz executor for the tool sandbox. Per SWARM.md: the swarm
;;; GENERATES a wide adversarial corpus; ONE loaded image (this file)
;;; RUNS it with a per-input timeout, catching what a cheap agent
;;; under-executes. crash / hang / injection = fail; a caught "TOOL
;;; ERROR" string = pass (the tool stayed robust).
;;;
;;;   sbcl --non-interactive --load inspect/fuzz-run.lisp -- [corpus-dir]
;;;
;;; A deterministic base corpus runs even with no swarm dir, so this is
;;; a self-contained oracle. Everything runs inside a throwaway sandbox
;;; (cwd), and Grep/Glob inputs with absolute paths are skipped so the
;;; fuzz can never scan the real filesystem.

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.fuzz
  (:use #:cl)
  (:local-nicknames (#:tools #:operandi.tools)
                    (#:llm   #:operandi.llm)
                    (#:jzon  #:com.inuoe.jzon)))
(in-package #:operandi.fuzz)

(defparameter *per-input-timeout* 5)
(defparameter *allowed* '("Read" "Grep" "Glob" "TodoWrite" "Write" "Edit"))
(defparameter *canary* "FUZZ_CANARY")

(defvar *sandbox*)

(defun setup-sandbox ()
  (let ((dir (merge-pathnames "operandi-fuzz-sandbox/"
                              (uiop:temporary-directory))))
    (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist dir)
    ;; fixtures the corpus can point Read at
    (with-open-file (s (merge-pathnames "big.txt" dir) :direction :output
                       :if-exists :supersede) (dotimes (i 200000) (write-line "aaaaaaaaaaaaaaaa" s)))
    (with-open-file (s (merge-pathnames "bin.dat" dir) :direction :output
                       :element-type '(unsigned-byte 8) :if-exists :supersede)
      (dotimes (i 4096) (write-byte (mod (* i 37) 256) s)))
    (with-open-file (s (merge-pathnames "longline.txt" dir) :direction :output
                       :if-exists :supersede) (dotimes (i 500000) (write-char #\x s)))
    (with-open-file (s (merge-pathnames "empty.txt" dir) :direction :output
                       :if-exists :supersede))
    (ensure-directories-exist (merge-pathnames "sub/" dir))
    (setf *sandbox* dir)
    (uiop:chdir dir)
    (setf *default-pathname-defaults* dir)
    dir))

(defun ht (&rest pairs) (apply #'llm:ht pairs))

(defun base-corpus ()
  "Known-nasty inputs, guaranteed to run regardless of the swarm output."
  (list
   ;; --- Read: non-regular / infinite / huge / extreme args ---
   (list "Read" (ht "path" "/dev/zero"))            ; no newline, no EOF -> infinite line
   (list "Read" (ht "path" "/dev/random"))
   (list "Read" (ht "path" "."))                    ; a directory
   (list "Read" (ht "path" "bin.dat"))              ; binary
   (list "Read" (ht "path" "longline.txt"))         ; one 500k-char line
   (list "Read" (ht "path" "big.txt" "limit" 1000000000))
   (list "Read" (ht "path" "big.txt" "offset" -5))
   (list "Read" (ht "path" "big.txt" "max_chars" 0))
   (list "Read" (ht "path" "big.txt" "offset" "notanumber")) ; wrong type
   (list "Read" (ht "path" 12345))                  ; path not a string
   ;; --- Grep / Glob: SHELL INJECTION probes (benign canary) ---
   (list "Grep" (ht "pattern" (format nil "x$(touch ~A)" *canary*) "path" "."))
   (list "Grep" (ht "pattern" (format nil "x`touch ~A`" *canary*) "path" "."))
   (list "Grep" (ht "pattern" "x" "path" (format nil ".;touch ~A" *canary*)))
   (list "Glob" (ht "pattern" (format nil "*$(touch ~A)" *canary*)))
   (list "Glob" (ht "pattern" (format nil "* ; touch ~A" *canary*)))
   ;; --- Grep / Glob: pathological patterns (relative, sandbox-bounded) ---
   (list "Grep" (ht "pattern" "/(a+)+$" "path" "."))
   (list "Grep" (ht "pattern" (make-string 20000 :initial-element #\a) "path" "."))
   (list "Grep" (ht "pattern" 42 "path" "."))       ; wrong type
   (list "Glob" (ht "pattern" "**/**/**/*"))
   (list "Glob" (ht "pattern" ""))
   ;; --- TodoWrite: malformed ---
   (list "TodoWrite" (ht "todos" "not-an-array"))
   (list "TodoWrite" (ht "todos" (vector "bare-string" 42)))
   (list "TodoWrite" (ht))                          ; missing required
   ;; --- Write / Edit: wrong types, empty/oversized, missing target ---
   (list "Write" (ht "path" "out.txt" "content" "hello world"))
   (list "Write" (ht "path" "out.txt" "content" 12345))     ; content not a string
   (list "Write" (ht "path" 999 "content" "x"))             ; path not a string
   (list "Write" (ht "content" "no path"))                  ; missing path
   (list "Edit" (ht "path" "out.txt" "old" "" "new" "z"))   ; empty OLD
   (list "Edit" (ht "path" "out.txt" "old" 5 "new" "z"))    ; OLD not a string
   (list "Edit" (ht "path" "nope.txt" "old" "a" "new" "b")) ; target missing
   (list "Edit" (ht "path" "out.txt" "old" "hello" "new" "goodbye")))) ; happy

(defun load-swarm-corpus (dir)
  "Parse every *.jsonl line in DIR into (tool args-ht). Malformed lines
   and non-object entries are skipped (they just don't run)."
  (let ((acc '()))
    (dolist (f (directory (merge-pathnames "*.jsonl" dir)))
      (handler-case
          (with-open-file (s f)
            (loop for line = (read-line s nil :eof)
                  until (eq line :eof)
                  when (plusp (length (string-trim '(#\Space #\Tab #\Return) line)))
                  do (handler-case
                         (let* ((o (jzon:parse line))
                                (tool (and (hash-table-p o) (gethash "tool" o)))
                                (args (and (hash-table-p o) (gethash "args" o))))
                           (when (and (stringp tool) (hash-table-p args))
                             (push (list tool args) acc)))
                       (error () nil))))
        (error () nil)))
    (nreverse acc)))

(defun unsafe-shellpath-p (args)
  "Skip Grep/Glob inputs that point at an absolute path — never let the
   fuzz spawn a scan of the real filesystem root."
  (let ((p (or (gethash "path" args) (gethash "pattern" args))))
    (and (stringp p) (plusp (length p)) (char= (char p 0) #\/))))

(defun confine-path (args)
  "Rewrite the :path arg to a plain STRING under the sandbox so a fuzzed
   Write/Edit can never touch a real file. Built by string concatenation,
   NOT merge-pathnames — an untrusted segment like `[x]` would otherwise
   be parsed as a wild-pathname pattern and blow up the harness."
  (let ((p (gethash "path" args)))
    (when (stringp p)
      (let* ((clean (remove-if (lambda (seg) (or (string= seg "") (string= seg "..")))
                               (uiop:split-string p :separator "/")))
             (rel (format nil "~{~A~^/~}" clean)))
        (setf (gethash "path" args)
              (concatenate 'string (namestring *sandbox*)
                           (if (plusp (length rel)) rel "fuzz-out")))))
    args))

(defun classify (tool args)
  "Run one input. Returns one of :ok :hang :stack :crash :injection :skip."
  (when (or (not (member tool *allowed* :test #'string=))
            (and (member tool '("Grep" "Glob") :test #'string=)
                 (unsafe-shellpath-p args)))
    (return-from classify :skip))
  ;; Write/Edit mutate files — force their target under the sandbox.
  ;; If confinement can't be done, SKIP rather than risk an escaped write.
  (when (member tool '("Write" "Edit") :test #'string=)
    (unless (ignore-errors (confine-path args) t)
      (return-from classify :skip)))
  (ignore-errors (delete-file (merge-pathnames *canary* *sandbox*)))
  (let ((result
          (handler-case
              (sb-ext:with-timeout *per-input-timeout*
                (progn (tools:invoke-tool tool args) :ok))
            (sb-ext:timeout () :hang)
            (storage-condition () :stack)
            (serious-condition () :crash))))
    ;; injection trumps a benign return: did a tool-arg run a shell command?
    (if (probe-file (merge-pathnames *canary* *sandbox*))
        (progn (ignore-errors (delete-file (merge-pathnames *canary* *sandbox*)))
               :injection)
        result)))

(defun short (tool args)
  (let ((s (handler-case (jzon:stringify args) (error () "?"))))
    (format nil "~A ~A" tool (subseq s 0 (min 90 (length s))))))

(defun run (&optional corpus-dir)
  (setup-sandbox)
  (let* ((swarm (if corpus-dir
                    (load-swarm-corpus (uiop:ensure-directory-pathname corpus-dir)) '()))
         (corpus (append (base-corpus) swarm))
         (tally (make-hash-table))
         (fails '()))
    (format t "~&fuzzing ~D inputs (~D base + ~D swarm)~%"
            (length corpus) (length (base-corpus)) (length swarm))
    (loop for (tool args) in corpus
          for verdict = (classify tool args)
          do (incf (gethash verdict tally 0))
             (unless (member verdict '(:ok :skip))
               (push (list verdict (short tool args)) fails)))
    (format t "~&~%results:~%")
    (loop for k in '(:ok :skip :hang :stack :crash :injection)
          do (format t "  ~10A ~D~%" k (gethash k tally 0)))
    (when fails
      (format t "~%FAILURES (crash/hang/injection):~%")
      (dolist (f (reverse fails)) (format t "  [~A] ~A~%" (first f) (second f))))
    (let ((bad (+ (gethash :hang tally 0) (gethash :stack tally 0)
                  (gethash :crash tally 0) (gethash :injection tally 0))))
      (format t "~%~D robustness failures~%" bad)
      (zerop bad))))

(let* ((args (remove "--" (uiop:command-line-arguments) :test #'string=))
       (ok (run (first args))))
  (sb-ext:exit :code (if ok 0 1)))
