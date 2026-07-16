;;; inspect/tool-context-test.lisp
;;;
;;; Oracle for tool-result context UX: the head+tail elision envelope
;;; (operandi.text:bound-result) and its application to the tools. The
;;; load-bearing property is TAIL RETENTION — a big result must keep its
;;; END (a command's verdict, an Eval's return value), not just its head.
;;; Under the old head-only cut every "keeps the tail" assertion here fails.
;;;
;;; Exit 0 iff all checks pass; doubles as a swarm/fitness oracle.

(require :asdf)
(funcall (read-from-string "ql:quickload") :operandi :silent t)
(funcall (find-symbol "OPEN-STORE" "OPERANDI.STORE"))

(defpackage #:tool-context-test
  (:use #:cl)
  (:local-nicknames (#:txt #:operandi.text) (#:tools #:operandi.tools)
                    (#:llm #:operandi.llm) (#:sf #:operandi.safefetch)))
(in-package #:tool-context-test)

(defvar *fails* 0)
(defmacro check (name form)
  `(handler-case (if ,form (format t "  ok   ~A~%" ,name)
                     (progn (incf *fails*) (format t "  FAIL ~A~%" ,name)))
     (error (e) (incf *fails*) (format t "  ERR  ~A: ~A~%" ,name e))))

(defun call (tool &rest kv)
  "Invoke TOOL with a string-keyed arg hash built from KV."
  (tools:invoke-tool tool (apply #'llm:ht kv)))

;;; ---- bound-result unit properties ----
(format t "~&== bound-result envelope ==~%")
(let ((short "just a little output"))
  (check "short string unchanged" (string= short (txt:bound-result short))))

(let* ((head (make-string 40000 :initial-element #\H))
       (tail (make-string 40000 :initial-element #\T))
       ;; a unique sentinel at the very END must survive
       (s (concatenate 'string head (string #\Newline) tail "TAIL_SENTINEL_ZZZ"))
       (b (txt:bound-result s :budget 20000 :label "output" :hint "do X")))
  (check "over-budget is shortened"   (< (length b) (length s)))
  (check "keeps some head"            (find #\H b))
  (check "keeps the TAIL sentinel"    (search "TAIL_SENTINEL_ZZZ" b))
  (check "self-describing elision"    (search "elided" b))
  (check "carries the recovery hint"  (search "do X" b))
  (check "roughly within budget"      (<= (length b) 21000)))

;; head-frac tilts the split: a low frac keeps MORE tail than head
(let* ((s (with-output-to-string (o)
            (dotimes (i 5000) (format o "L~4,'0D ................................~%" i))))
       (tail-heavy (txt:bound-result s :budget 4000 :head-frac 0.2d0))
       (head-heavy (txt:bound-result s :budget 4000 :head-frac 0.8d0)))
  ;; last line index present in tail-heavy, early line index present in head-heavy
  (check "low head-frac keeps the end"  (search "L4999" tail-heavy))
  (check "high head-frac keeps the end" (search "L4999" head-heavy))   ; tail always kept
  (check "high head-frac keeps earlier head than low"
         (> (or (search "L02" head-heavy) 0) -1)))

;;; ---- Bash keeps the verdict (tail) ----
(format t "~&== Bash tail retention ==~%")
(let ((r (call "Bash" "command"
               "for i in $(seq 1 4000); do echo \"xxxxxxxxxxxxxxxxxxxxxxxxxxxx line $i\"; done; echo VERDICT_SENTINEL_9Z")))
  (check "bash output was bounded"   (< (length r) 60000))
  (check "bash kept the VERDICT tail" (search "VERDICT_SENTINEL_9Z" r))
  (check "bash noted the elision"    (search "elided" r)))

;;; ---- Eval always keeps the => value, even under noisy stdout ----
(format t "~&== Eval value preservation ==~%")
(let ((r (call "Eval" "form"
               "(progn (dotimes (i 4000) (format t \"noise noise noise noise line ~D~%\" i)) (list :answer 42 :sentinel :EVAL_VALUE_KEPT))")))
  (check "eval kept the => value under noise" (search "EVAL_VALUE_KEPT" r))
  (check "eval shows the => marker"           (search "=>" r))
  (check "eval bounded the stdout"            (< (length r) 80000)))

(let ((r (call "Eval" "form"
               "(progn (dotimes (i 4000) (format t \"line ~D~%\" i)) (error \"boom-sentinel-xyz\"))")))
  (check "eval kept the ERROR under noise" (and (search "EVAL ERROR" r)
                                                (search "boom-sentinel-xyz" r))))

;;; ---- Grep tells the agent when it hit the wall ----
(format t "~&== Grep narrow hint ==~%")
(let ((r (call "Grep" "pattern" "(" "path" "src")))
  (check "grep signals the 200-line wall" (search "matching lines total" r)))

;;; ---- WebFetch envelope keeps the tail (unit, no network) ----
(format t "~&== WebFetch cap keeps the tail ==~%")
(let* ((page (concatenate 'string (make-string 60000 :initial-element #\p) "PAGE_TAIL_KEEP"))
       (r (funcall (find-symbol "%CAP-FETCH" "OPERANDI.SAFEFETCH") page)))
  (check "fetch cap keeps the page tail" (search "PAGE_TAIL_KEEP" r))
  (check "fetch cap bounded the page"    (< (length r) (length page))))

(format t "~&~%tool-context-test: ~A failure~:P~%" *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
