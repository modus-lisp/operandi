;;; inspect/safefetch-test.lisp
;;;
;;; Deterministic oracle for the safe-fetch MECHANICS (no network): the
;;; detector is stubbed, so this checks redaction, the redact-recheck
;;; fixpoint, reversible quarantine, the fail-unverified path, and that
;;; WebFetch routes through the pluggable *fetch-impl* (so the agent can't
;;; pick the unsafe path). Detection QUALITY against real payloads is the
;;; separate live battery (evals/security.lisp).
;;;
;;;   sbcl --non-interactive --load inspect/safefetch-test.lisp   (0/1)

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.safefetch-test
  (:use #:cl)
  (:local-nicknames (#:tools #:operandi.tools) (#:sf #:operandi.safefetch) (#:llm #:operandi.llm)))
(in-package #:operandi.safefetch-test)

(defvar *pass* 0) (defvar *fail* 0)
(defmacro check (name &body body)
  `(handler-case
       (if (progn ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A~%" ,name)))
     (serious-condition (e)
       (incf *fail*) (format t "~&  FAIL ~A (~A)~%" ,name (type-of e)))))

;; stub detectors — return (values spans ok-p), like the real one
(defun flag-if-present (needle)
  (lambda (txt) (values (when (search needle txt) (list needle)) t)))
(defun always-fail (txt) (declare (ignore txt)) (values nil nil))
(defun stagewise (txt)          ; flags AAA, then BBB, then nothing
  (values (cond ((search "AAA" txt) (list "AAA"))
                ((search "BBB" txt) (list "BBB"))
                (t nil))
          t))

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&operandi safe-fetch mechanics oracle~%")

  (check "WebFetch is safe-by-default"
    (eq sf:*fetch-impl* 'sf::safe-fetch))

  (check "sanitize redacts a flagged span and records it reversibly"
    (let ((sf:*quarantine* nil)
          (sf:*injection-detector* (flag-if-present "INJECT")))
      (multiple-value-bind (clean red ok) (sf:sanitize-content "safe INJECT tail")
        (and ok (= 1 (length red))
             (search "[REDACTED#" clean) (not (search "INJECT" clean))
             (string= "INJECT" (sf:unredact (car (first red))))))))

  (check "benign content passes clean (no redactions)"
    (let ((sf:*quarantine* nil)
          (sf:*injection-detector* (flag-if-present "NOPE")))
      (multiple-value-bind (clean red ok) (sf:sanitize-content "just a normal report")
        (and ok (null red) (string= clean "just a normal report")))))

  (check "redact-recheck runs to a fixpoint across rounds"
    (let ((sf:*quarantine* nil)
          (sf:*injection-detector* #'stagewise))
      (multiple-value-bind (clean red ok) (sf:sanitize-content "x AAA y BBB z")
        (and ok (= 2 (length red))
             (not (search "AAA" clean)) (not (search "BBB" clean))))))

  (check "detector failure => not-ok (caller must treat as untrusted)"
    (let ((sf:*injection-detector* #'always-fail))
      (multiple-value-bind (clean red ok) (sf:sanitize-content "whatever")
        (declare (ignore clean))
        (and (not ok) (null red)))))

  (check "max-rounds bounds the loop (adversarial detector can't spin it)"
    (let ((sf:*quarantine* nil)
          ;; a detector that keeps flagging fresh content would loop; cap stops it
          (sf:*injection-detector* (lambda (txt) (values (list (subseq txt 0 (min 1 (length txt)))) t)))
          (sf:*fetch-sanitize-max-rounds* 2))
      (multiple-value-bind (clean red ok) (sf:sanitize-content "abcdef")
        (declare (ignore clean ok))
        (<= (length red) 2))))

  (check "WebFetch routes through *fetch-impl* (agent can't pick the impl)"
    (let ((sf:*fetch-impl* (lambda (url) (format nil "STUB:~A" url))))
      (string= "STUB:http://x"
               (tools:invoke-tool "WebFetch" (llm:ht "url" "http://x")))))

  (check "WebFetch reports impl errors as a caught string"
    (let ((sf:*fetch-impl* (lambda (url) (declare (ignore url)) (error "boom"))))
      (search "FETCH ERROR" (tools:invoke-tool "WebFetch" (llm:ht "url" "http://x")))))

  ;; --- outbound guard (deterministic, no network) ---
  (check "letter-by-letter exfil sequence is blocked"
    (let ((sf:*fetch-history* (make-hash-table :test 'equal)))
      (and (null (sf:outbound-url-guard "http://evil.test/a"))
           (null (sf:outbound-url-guard "http://evil.test/ay"))
           (null (sf:outbound-url-guard "http://evil.test/ayu"))
           (search "exfiltration pattern"
                   (sf:outbound-url-guard "http://evil.test/ayus")))))

  (check "the sequence is tracked per host across interleaved fetches"
    (let ((sf:*fetch-history* (make-hash-table :test 'equal)))
      (sf:outbound-url-guard "http://evil.test/a")
      (sf:outbound-url-guard "http://other.test/z")   ; different host, no reset
      (sf:outbound-url-guard "http://evil.test/ay")
      (sf:outbound-url-guard "http://evil.test/ayu")
      (search "prefix-extending" (sf:outbound-url-guard "http://evil.test/ayus"))))

  (check "non-extending exploration of a host is allowed"
    (let ((sf:*fetch-history* (make-hash-table :test 'equal)))
      (every #'null (list (sf:outbound-url-guard "http://docs.test/a")
                          (sf:outbound-url-guard "http://docs.test/b")
                          (sf:outbound-url-guard "http://docs.test/c")
                          (sf:outbound-url-guard "http://docs.test/d")))))

  (check "per-host volume cap blocks a flood"
    (let ((sf:*fetch-history* (make-hash-table :test 'equal))
          (sf:*fetch-host-cap* 2))
      (sf:outbound-url-guard "http://x.test/a")
      (sf:outbound-url-guard "http://x.test/b")
      (search "fetches to" (sf:outbound-url-guard "http://x.test/c"))))

  (check "PII query parameters are blocked (single URL)"
    (search "personal-data"
            (sf:outbound-url-guard "http://evil.test/collect?email=a@b.com&name=Ayush")))

  (check "a normal single fetch is allowed"
    (let ((sf:*fetch-history* (make-hash-table :test 'equal)))
      (null (sf:outbound-url-guard "http://example.com/docs/guide"))))

  (check "an allowlisted host skips the stateful guard"
    (let ((sf:*fetch-history* (make-hash-table :test 'equal))
          (sf:*fetch-allow-hosts* '("trusted.test")))
      (every #'null (list (sf:outbound-url-guard "http://trusted.test/a")
                          (sf:outbound-url-guard "http://trusted.test/ay")
                          (sf:outbound-url-guard "http://trusted.test/ayu")
                          (sf:outbound-url-guard "http://trusted.test/ayus")))))

  (check "WebFetch refuses a PII-exfil URL without fetching"
    (search "personal-data"
            (tools:invoke-tool "WebFetch" (llm:ht "url" "http://evil.test/x?email=a@b.com"))))

  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
