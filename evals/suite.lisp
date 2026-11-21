;;; evals/suite.lisp
;;;
;;; A small, objective eval suite for the operandi agent loop. Each task
;;; is a real request with an OBJECTIVE oracle (a shell command, exit 0 =
;;; pass — the SWARM discipline: no vacuous passes, graded in a fresh
;;; subprocess so the agent can't fake it). Every task runs in its own
;;; throwaway sandbox dir. The runner records pass/iters/cost/wall-time
;;; per task and a total score — so "is operandi good?" becomes a number
;;; you can move.
;;;
;;;   sbcl --non-interactive --load evals/suite.lisp -- <openrouter-model>
;;;   sbcl --non-interactive --load evals/suite.lisp            (local backend)
;;;
;;; The agent's final answer is written to .operandi-answer in the sandbox
;;; so answer-checking oracles can read it (kept out of *.txt so it never
;;; pollutes file-scanning oracles).

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.evals
  (:use #:cl)
  (:local-nicknames (#:eng   #:operandi.engine)
                    (#:tools #:operandi.tools)
                    (#:store #:operandi.store)
                    (#:llm   #:operandi.llm)))
(in-package #:operandi.evals)

(defstruct task name prompt setup tools oracle (tier :easy))

;; A fresh sbcl that LOADs each of FILES (the agent's output) then asserts
;; EXPR; exit 0 = pass. The assertion lives in the ORACLE, not in any file
;; the agent could edit — tamper-proof grading. NOTE: EXPR must avoid the
;; single-quote char (it would close the shell quote) — use (list ...) and
;; (quote sym) instead of '(...) / 'sym.
(defun lisp-check* (files expr)
  (with-output-to-string (s)
    (write-string "sbcl --non-interactive --disable-debugger " s)
    (dolist (f files) (format s "--eval '(load ~S)' " f))
    (format s "--eval '(handler-case (sb-ext:exit :code (if ~A 0 1)) (error () (sb-ext:exit :code 1)))' 2>/dev/null"
            expr)))
(defun lisp-check (file expr) (lisp-check* (list file) expr))

(defparameter *tasks*
  (list
   (make-task
    :name "implement-fib"
    :prompt "Create a file fib.lisp in the current directory that defines a
function named FIB with (fib 0)=0, (fib 1)=1, (fib n)=(fib (- n 1))+(fib (- n 2)).
Then load it and confirm (fib 10) returns 55."
    :oracle (lisp-check "fib.lisp" "(and (eql (fib 0) 0) (eql (fib 7) 13) (eql (fib 10) 55))"))

   (make-task
    :name "fix-bug"
    :setup "printf '(defun mul (a b) (+ a b))\\n' > math.lisp"
    :prompt "The file math.lisp defines MUL, which is supposed to MULTIPLY its two
arguments but has a bug (it adds). Fix math.lisp so MUL multiplies correctly, then
confirm (mul 6 7) is 42."
    :oracle (lisp-check "math.lisp" "(and (eql (mul 6 7) 42) (eql (mul 3 4) 12))"))

   (make-task
    :name "edit-config"
    :setup "printf 'host = localhost\\nport = 8080\\ntimeout = 30\\n' > server.conf"
    :prompt "In server.conf, change the port from 8080 to 9090. Leave every other
line exactly as it is."
    :oracle "grep -qx 'port = 9090' server.conf && grep -qx 'host = localhost' server.conf && grep -qx 'timeout = 30' server.conf")

   (make-task
    :name "rename-refactor"
    :setup "printf 'the fooWidget calls fooWidget again\\n' > a.txt; printf 'another fooWidget lives here\\n' > b.txt"
    :prompt "Rename every occurrence of 'fooWidget' to 'barGadget' across all .txt
files in the current directory."
    :oracle "! grep -rq fooWidget --include='*.txt' . && [ \"$(grep -rho barGadget --include='*.txt' . | wc -l)\" -eq 3 ]")

   (make-task
    :name "count-defs"
    :setup "printf 'def a():\\n pass\\ndef b():\\n pass\\ndef c():\\n pass\\n' > x.py; printf 'def d():\\n pass\\ndef e():\\n pass\\n' > y.py; printf 'def f():\\n pass\\ndef g():\\n pass\\n' > z.py"
    :prompt "How many Python function definitions (lines beginning with 'def ')
are there in total across all .py files in the current directory? Answer with
just the number."
    :oracle "grep -qE '(^|[^0-9])7([^0-9]|$)' .operandi-answer")

   (make-task
    :name "sum-lines"
    :setup "printf 'a\\nb\\nc\\n' > f1.txt; printf 'x\\ny\\n' > f2.txt"
    :prompt "How many lines are there in total across all .txt files in the
current directory? Answer with just the number."
    :oracle "grep -qE '(^|[^0-9])5([^0-9]|$)' .operandi-answer")

   (make-task
    :name "reverse-string"
    :prompt "Create rev.lisp defining a function REV that takes a string and
returns it reversed. Confirm (rev \"hello\") returns \"olleh\"."
    :oracle (lisp-check "rev.lisp" "(and (string= (rev \"hello\") \"olleh\") (string= (rev \"abc\") \"cba\"))"))

   (make-task
    :name "extract-value"
    :setup "printf '{\"name\":\"svc\",\"port\":7654,\"debug\":true}\\n' > config.json"
    :prompt "What is the value of the \"port\" field in config.json? Answer with
just the number."
    :oracle "grep -qE '(^|[^0-9])7654([^0-9]|$)' .operandi-answer")

   ;; ------------------------------ hard ------------------------------
   (make-task
    :tier :hard
    :name "impl-stack"
    :setup "printf ';;; Implement these four stubs, a stack backed by the list *STACK*.\\n(defvar *stack* (list))\\n(defun stk-push (x) (error \"todo\"))\\n(defun stk-pop () (error \"todo\"))\\n(defun stk-peek () (error \"todo\"))\\n(defun stk-size () (error \"todo\"))\\n' > stack.lisp"
    :prompt "stack.lisp has four stub functions backed by the list *STACK*:
stk-push (push a value), stk-pop (remove AND return the top), stk-peek (return
the top without removing), stk-size (count). Implement all four as a LIFO stack.
Verify by pushing 10, 20, 30 and checking pop/peek/size."
    :oracle (lisp-check "stack.lisp"
                        "(progn (setf *stack* (list)) (stk-push 10) (stk-push 20) (stk-push 30) (and (= (stk-size) 3) (= (stk-peek) 30) (= (stk-pop) 30) (= (stk-pop) 20) (= (stk-size) 1) (= (stk-peek) 10)))"))

   (make-task
    :tier :hard
    :name "multifile-bug"
    :setup "printf '(defun my-sum (xs) (reduce (function +) xs :initial-value 1))\\n' > sum.lisp; printf '(defun mean (xs) (/ (my-sum xs) (length xs)))\\n' > stats.lisp"
    :prompt "Calling (mean (list 2 4 6)) should give 4 but returns the wrong
value. The bug is NOT in MEAN itself — it's in a function MEAN depends on (look
at stats.lisp and sum.lisp). Find and fix it. Do not change MEAN."
    :oracle (lisp-check* (list "sum.lisp" "stats.lisp")
                         "(and (= (mean (list 2 4 6)) 4) (= (my-sum (list 1 2 3)) 6))"))

   (make-task
    :tier :hard
    :name "edge-case"
    :setup "printf '(defun safe-div (a b) (/ a b))\\n' > safe-div.lisp"
    :prompt "Make SAFE-DIV in safe-div.lisp return the keyword :UNDEFINED when
the divisor is 0, instead of signalling an error. Ordinary division must still
work normally."
    :oracle (lisp-check "safe-div.lisp"
                        "(and (= (safe-div 10 2) 5) (eq (safe-div 5 0) :undefined) (= (safe-div 9 3) 3))"))

   (make-task
    :tier :hard
    :name "refactor-preserve"
    :setup "printf '(defun describe-circle (r) (format nil \"circle area=~,2F perimeter=~,2F\" (* pi r r) (* 2 pi r)))\\n(defun describe-ball (r) (format nil \"ball area=~,2F perimeter=~,2F\" (* pi r r) (* 2 pi r)))\\n' > shapes.lisp"
    :prompt "describe-circle and describe-ball in shapes.lisp duplicate the area
and perimeter formulas. Extract the two formulas into helper functions named
CIRCLE-AREA and CIRCLE-PERIMETER and call them from both. The strings the two
functions return must stay byte-for-byte identical."
    :oracle (lisp-check "shapes.lisp"
                        "(and (fboundp (quote circle-area)) (fboundp (quote circle-perimeter)) (string= (describe-circle 2) \"circle area=12.57 perimeter=12.57\") (string= (describe-ball 3) \"ball area=28.27 perimeter=18.85\"))"))

   (make-task
    :tier :hard
    :name "call-counting"
    :setup "printf '(log-event 1)(log-event 2)(process 3)\\n' > f1.lisp; printf '(log-event 4)(process 5)(process 6)\\n' > f2.lisp; printf '(log-event 7)(validate 8)\\n' > f3.lisp; printf '(log-event 9)(validate 10)(validate 11)\\n' > f4.lisp; printf '(process 12)(validate 13)\\n' > f5.lisp"
    :prompt "Across all .lisp files in this directory, which function is CALLED
the most times (count call sites like (name ...) ), and how many times? Answer
with exactly 'NAME: COUNT'."
    :oracle "grep -qi log-event .operandi-answer && grep -qE '(^|[^0-9])5([^0-9]|$)' .operandi-answer")

   ;; ------------------------------ tools ------------------------------
   ;; The answer cannot be known without using tools: nothing to write that
   ;; merely looks right, only something to find out.
   (make-task
    :tier :tools
    :name "pointer-chain"
    :setup "mkdir -p notes archive/2019; printf 'The next clue is in notes/second.txt\\n' > start.txt; printf 'Keep going: archive/2019/third.txt\\n' > notes/second.txt; printf 'The code word is PERIWINKLE-42.\\n' > archive/2019/third.txt"
    :prompt "Follow the clues starting at start.txt in the current directory and
tell me the code word at the end of the trail."
    :oracle "grep -q PERIWINKLE-42 .operandi-answer")

   (make-task
    :tier :tools
    :name "find-needle"
    :setup "for d in a b c d e; do mkdir -p logs/$d; for i in 1 2 3 4 5 6; do printf 'INFO service started\\nINFO request ok\\n' > logs/$d/f$i.log; done; done; printf 'INFO service started\\nERROR 4711 disk quota exceeded\\n' > logs/c/f4.log"
    :prompt "One of the log files under logs/ contains error code 4711. Which
file is it? Answer with its path."
    :oracle "grep -q 'c/f4.log' .operandi-answer")

   (make-task
    :tier :tools
    :name "exact-arith"
    :prompt "What is 37 raised to the power 41, modulo 1000003? I need the exact
value, so compute it rather than estimating. Answer with just the number."
    :oracle "grep -qE '(^|[^0-9])603636([^0-9]|$)' .operandi-answer")

   (make-task
    :tier :tools
    :name "csv-filter-sum"
    :setup "{ echo 'id,region,amount'; for i in $(seq 1 40); do case $((i % 3)) in 0) r=west;; 1) r=east;; 2) r=north;; esac; echo \"$i,$r,$(( (i * 37) % 101 ))\"; done; } > sales.csv"
    :prompt "sales.csv has columns id, region and amount. What is the total
amount over the rows whose region is west? Answer with just the number."
    :oracle "grep -qE '(^|[^0-9])607([^0-9]|$)' .operandi-answer")

   (make-task
    :tier :tools
    :name "count-word"
    :setup "for i in $(seq 1 30); do l=\"line $i:\"; [ $((i % 3)) -eq 0 ] && l=\"$l the old lantern glowed\"; [ $((i % 5)) -eq 0 ] && l=\"$l and a LANTERN swung\"; echo \"$l\"; done > story.txt"
    :prompt "How many times does the word 'lantern' occur in story.txt, ignoring
case? Count occurrences, not lines. Answer with just the number."
    :oracle "grep -qE '(^|[^0-9])16([^0-9]|$)' .operandi-answer")))

(defparameter *core-tools* '("Eval" "Read" "Write" "Edit" "Bash" "Grep" "Glob")
  "--tools core: the file and shell tools these tasks need, without the
   delegation, web and memory tools that the full set adds to every prompt.")

(defvar *tool-names* nil "The tools every task gets; NIL = the task's own, else the default set.")
(defvar *transcripts* nil "A directory each task's messages are written to as NAME.json, or NIL.")

(defun count-tool-calls (messages)
  (loop for m in messages
        for calls = (and (hash-table-p m) (gethash "tool_calls" m))
        sum (if (vectorp calls) (length calls) (length (if (listp calls) calls nil)))))

(defun run-one (task base)
  (let* ((dir (merge-pathnames (concatenate 'string (task-name task) "/") base))
         (t0 (get-internal-real-time)))
    (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist dir)
    (when (task-setup task)
      (uiop:run-program (list "bash" "-c" (task-setup task))
                        :directory dir :ignore-error-status t))
    (uiop:chdir dir)
    (setf *default-pathname-defaults* dir)
    (multiple-value-bind (text messages iters usage)
        (handler-case
          ;; a fresh package for the task's Eval and what it LOADs: tasks run
          ;; in one image, and one task's FIB or *STACK* must not be found by
          ;; the next
          (let ((tools:*eval-package*
                  (let ((name (format nil "EVAL-~:@(~A~)" (task-name task))))
                    (when (find-package name) (delete-package name))
                    (make-package name :use '(:cl)))))
            (eng:run (task-prompt task)
                     :tool-names (or (task-tools task) *tool-names* (tools:default-tools))
                     :max-iterations 30 :verbose nil))
          (error (e) (values (format nil "RUN ERROR: ~A" e) nil 0 (llm:make-usage))))
      (let ((pkg (find-package (format nil "EVAL-~:@(~A~)" (task-name task)))))
        (when pkg (delete-package pkg)))
      (with-open-file (s (merge-pathnames ".operandi-answer" dir)
                         :direction :output :if-exists :supersede
                         :if-does-not-exist :create)
        (write-string (or text "") s))
      (when *transcripts*
        (with-open-file (s (merge-pathnames (format nil "~A.json" (task-name task)) *transcripts*)
                           :direction :output :if-exists :supersede :if-does-not-exist :create)
          (write-string (json-simple:stringify (coerce (or messages (list (llm:ht "error" (or text "")))) 'vector)) s)))
      (let* ((exit (nth-value 2 (uiop:run-program (list "bash" "-c" (task-oracle task))
                                                  :directory dir :output nil
                                                  :error-output nil :ignore-error-status t)))
             (secs (/ (- (get-internal-real-time) t0)
                      internal-time-units-per-second)))
        (list :name (task-name task) :pass (eql exit 0)
              :iters (or iters 0) :calls (count-tool-calls messages)
              :cost (llm:usage-cost-usd usage)
              :cached (llm:usage-cached-tokens usage)
              :prompt (llm:usage-prompt-tokens usage) :secs secs
              :answer (let ((a (or text ""))) (subseq a 0 (min 200 (length a)))))))))

(defun run-evals (&key model (tasks *tasks*))
  (when model (llm:set-backend-openrouter! :model model))
  (store:open-store)
  (let* ((root (uiop:getcwd))
         (base (merge-pathnames "operandi-evals/" (uiop:temporary-directory)))
         (results (mapcar (lambda (task) (run-one task base)) tasks)))
    (uiop:chdir root)
    (format t "~&~%~68,,,'-A~%" "")
    (format t "~2Aeval~22T~8A~7A~7A~9A~8A~%" "" "result" "iters" "calls" "cost" "secs")
    (format t "~68,,,'-A~%" "")
    (dolist (r results)
      (format t "  ~20A ~8A ~5A  ~5A ~6,3F¢ ~6,1F~%"
              (getf r :name) (if (getf r :pass) "PASS" "fail")
              (getf r :iters) (getf r :calls) (* 100 (getf r :cost)) (getf r :secs)))
    (let* ((n (length results))
           (passed (count t results :key (lambda (r) (getf r :pass))))
           (cost (reduce #'+ results :key (lambda (r) (getf r :cost))))
           (secs (reduce #'+ results :key (lambda (r) (getf r :secs))))
           (prompt (reduce #'+ results :key (lambda (r) (getf r :prompt))))
           (cached (reduce #'+ results :key (lambda (r) (getf r :cached)))))
      (format t "~68,,,'-A~%" "")
      (format t "SCORE ~A/~A   total ~,2F¢   ~,1Fs   model ~A~%"
              passed n (* 100 cost) secs (or model llm:*llm-model* "local"))
      (when *transcripts*
        (with-open-file (s (merge-pathnames "results.json" *transcripts*)
                           :direction :output :if-exists :supersede :if-does-not-exist :create)
          (write-string (json-simple:stringify
                         (llm:ht "model" (or model llm:*llm-model* "local") "passed" passed "n" n "secs" secs
                                 "tasks" (coerce (mapcar (lambda (r)
                                                           (llm:ht "name" (getf r :name) "pass" (and (getf r :pass) t)
                                                                   "iters" (getf r :iters) "calls" (getf r :calls)
                                                                   "secs" (float (getf r :secs)) "prompt" (getf r :prompt)
                                                                   "answer" (getf r :answer)))
                                                         results)
                                                 'vector)))
                        s)))
      (format t "prompt tokens ~:D, ~:D cached (~D%% prefix-cache hit)~%"
              prompt cached (if (plusp prompt) (round (* 100 cached) prompt) 0))
      (zerop (- n passed)))))

;; args (any order): an openrouter model (contains "/"), and/or a suite
;; selector easy|hard|tools|all (default all).  Also:
;;   --illatio PATH   run the GGUF at PATH in this image (ILLATIO_GPU=1 for Metal)
;;   --local URL      a local OpenAI server (llama-server, illatio --serve) at URL
;;   --tools core     only *CORE-TOOLS* (default: operandi's full default set)
;;   --only a,b       only the tasks with these names
;;   --out DIR        write each task's transcript and results.json to DIR
;;   --keep-notes     keep ~/.operandi's notes in the prompt (default: none, so
;;                    a run measures the model, not whoever's machine it is on)
(let* ((args (remove "--" (uiop:command-line-arguments) :test #'string=))
       (opt (lambda (flag) (let ((p (member flag args :test #'string=))) (and p (second p)))))
       (illatio (funcall opt "--illatio"))
       (local (funcall opt "--local"))
       (only (let ((o (funcall opt "--only"))) (and o (uiop:split-string o :separator ","))))
       (out (funcall opt "--out"))
       (plain (loop with skip = nil for a in args
                    if skip do (setf skip nil)
                    else if (member a '("--illatio" "--local" "--tools" "--only" "--out") :test #'string=) do (setf skip t)
                    else if (uiop:string-prefix-p "--" a) do (progn)
                    else collect a))
       (model (find-if (lambda (a) (find #\/ a)) plain))
       (suite (or (find-if (lambda (a) (member a '("easy" "hard" "tools" "all") :test #'string-equal)) plain)
                  "all"))
       (tasks (if (string-equal suite "all")
                  *tasks*
                  (remove-if-not (lambda (tk) (string-equal (task-tier tk) suite)) *tasks*))))
  (when only
    (setf tasks (remove-if-not (lambda (tk) (member (task-name tk) only :test #'string=)) tasks)))
  (when (equal (funcall opt "--tools") "core") (setf *tool-names* *core-tools*))
  (unless (member "--keep-notes" args :test #'string=)
    (setf tools:*notes-file* (merge-pathnames "operandi-evals-no-notes.md" (uiop:temporary-directory))))
  (when out
    (setf *transcripts* (uiop:ensure-directory-pathname (merge-pathnames out (uiop:getcwd))))
    (ensure-directories-exist *transcripts*))
  (when local
    (llm:set-backend-llama! :url local)
    (format t "~&local: ~A at ~A~%" llm:*llm-model* local))
  (when illatio
    (llm:set-backend-illatio! :path illatio)
    (format t "~&illatio: ~A (context ~D)~%" llm:*llm-model* (llm:illatio-context-length)))
  (format t "~&suite=~A (~A tasks)~%" suite (length tasks))
  (sb-ext:exit :code (if (run-evals :model (and (not illatio) (not local) model) :tasks tasks) 0 1)))
