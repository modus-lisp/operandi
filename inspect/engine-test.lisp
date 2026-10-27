;;; inspect/engine-test.lisp
;;;
;;; Oracle for the agent loop's handling of empty / null-content turns.
;;; The chat API sometimes ends a turn with content=null and no tool
;;; call (a stall); json-simple parses that null to the symbol NULL, which used
;;; to leak out as the literal answer "NULL". The loop should instead
;;; nudge the model back to work and, failing that, salvage the last real
;;; content — never return "NULL"/"".
;;;
;;; We stub DO-CHAT-WITH-RETRIES with a scripted queue of responses, so
;;; the whole loop runs deterministically with no network.
;;;
;;;   sbcl --non-interactive --load inspect/engine-test.lisp   (exits 0/1)

(require :asdf)
(unless (find-package :ql)
  (let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file q) (load q))))
(funcall (read-from-string "ql:quickload") :operandi :silent t)

(defpackage #:operandi.engine-test
  (:use #:cl)
  (:local-nicknames (#:eng #:operandi.engine)
                    (#:llm #:operandi.llm))
  (:export #:run))
(in-package #:operandi.engine-test)

(defvar *pass* 0)
(defvar *fail* 0)
(defvar *script* nil)

(defmacro check (name &body body)
  `(handler-case
       (if (progn ,@body)
           (progn (incf *pass*) (format t "~&  ok   ~A~%" ,name))
           (progn (incf *fail*) (format t "~&  FAIL ~A~%" ,name)))
     (serious-condition (e)
       (incf *fail*) (format t "~&  FAIL ~A (~A)~%" ,name (type-of e)))))

;;; ---- scripted backend ----------------------------------------------

(defun assistant-resp (&key content tool-calls finish)
  "A parsed chat-completion response, shaped like the real API. CONTENT
   NIL becomes JSON null (the symbol NULL, as json-simple would produce).
   FINISH, if given, is the choice's finish_reason (\"length\" = cut off
   at the output cap)."
  (let ((m (llm:ht "role" "assistant" "content" (or content 'null))))
    (when tool-calls (setf (gethash "tool_calls" m) tool-calls))
    (let ((choice (llm:ht "message" m)))
      (when finish (setf (gethash "finish_reason" choice) finish))
      (llm:ht "choices" (vector choice)))))

(defun bash-call (id cmd)
  (llm:ht "id" id "type" "function"
          "function" (llm:ht "name" "Bash"
                             "arguments" (format nil "{\"command\":~S}" cmd))))

(defun install-mock (responses)
  "Make DO-CHAT-WITH-RETRIES pop from RESPONSES; when exhausted, keep
   returning a null-content turn (so a bug can't loop forever)."
  (setf *script* (copy-list responses))
  (setf (symbol-function 'operandi.engine::do-chat-with-retries)
        (lambda (messages tools-vec &key verbose)
          (declare (ignore messages tools-vec verbose))
          (or (pop *script*) (assistant-resp :content 'null)))))

(defun final (&rest responses)
  "Run the loop against a scripted response list; return its final text."
  (install-mock responses)
  (values (eng:run "do the task" :verbose nil)))

;;; ---- cases ---------------------------------------------------------

;;; ---- llama.cpp model discovery --------------------------------------
;;; --llama with no -m asks the server which model to use.  The picker is
;;; pure, so it is checked here against the shapes llama.cpp's router
;;; actually returns (status.value "loaded"/"unloaded", meta.size bytes).

(defun llama-model (id &key loaded size n-params top-size)
  (let ((m (llm:ht "id" id)))
    (when loaded (setf (gethash "status" m) (llm:ht "value" "loaded")))
    (when top-size (setf (gethash "size" m) top-size))
    (when (or size n-params)
      (let ((meta (llm:ht)))
        (when size (setf (gethash "size" meta) size))
        (when n-params (setf (gethash "n_params" meta) n-params))
        (setf (gethash "meta" m) meta)))
    m))

(defun run-llama-picks ()
  (format t "~&llama.cpp model discovery~%")

  (check "one model is taken as-is, no size needed"
    (multiple-value-bind (id reason)
        (llm::llama-pick-model (list (llama-model "solo")))
      (and (string= id "solo") (search "only model" reason))))

  (check "an empty list yields no model, not an error"
    (multiple-value-bind (id reason) (llm::llama-pick-model nil)
      (and (null id) (stringp reason))))

  (check "entries without a usable id are ignored"
    (multiple-value-bind (id reason)
        (llm::llama-pick-model (list (llm:ht "id" "") (llm:ht "id" 7)
                                     (llama-model "real")))
      (and (string= id "real") (search "only model" reason))))

  (check "several models: the largest wins"
    (multiple-value-bind (id reason)
        (llm::llama-pick-model (list (llama-model "small" :size 1000)
                                     (llama-model "big" :size 9000)
                                     (llama-model "mid" :size 5000)))
      (and (string= id "big") (search "largest" reason))))

  (check "the one already loaded wins over a larger unloaded one"
    (multiple-value-bind (id reason)
        (llm::llama-pick-model (list (llama-model "loaded" :loaded t :size 1000)
                                     (llama-model "huge" :size 9000)))
      (and (string= id "loaded") (search "already loaded" reason))))

  (check "with several loaded, size decides (no arbitrary preference)"
    (multiple-value-bind (id reason)
        (llm::llama-pick-model (list (llama-model "a" :loaded t :size 1000)
                                     (llama-model "b" :loaded t :size 9000)))
      (and (string= id "b") (search "largest" reason))))

  (check "no sizes reported: falls back to the first, not an error"
    (multiple-value-bind (id reason)
        (llm::llama-pick-model (list (llama-model "first") (llama-model "second")))
      (and (string= id "first") (stringp reason))))

  (check "n_params is a usable size when meta.size is absent"
    (multiple-value-bind (id reason)
        (llm::llama-pick-model (list (llama-model "few" :n-params 1000)
                                     (llama-model "many" :n-params 9000)))
      (and (string= id "many") (search "largest" reason))))

  (check "a top-level size is read too (some servers report it there)"
    (multiple-value-bind (id reason)
        (llm::llama-pick-model (list (llama-model "small" :top-size 1000)
                                     (llama-model "big" :top-size 9000)))
      (and (string= id "big") (search "largest" reason))))

  (check "a model with no size loses to one that has a size"
    (multiple-value-bind (id reason)
        (llm::llama-pick-model (list (llama-model "unsized")
                                     (llama-model "sized" :size 1)))
      (and (string= id "sized") (search "largest" reason))))

  (check "the models URL is the sibling of the chat URL"
    (and (string= "http://localhost:9931/v1/models"
                  (llm::llama-models-url "http://localhost:9931/v1/chat/completions"))
         (string= "http://box:9931/proxy/v1/models"
                  (llm::llama-models-url "http://box:9931/proxy/v1/chat/completions"))))

  (check "a colon-prefixed port becomes a chat URL; a path is kept"
    (and (string= "http://localhost:9931/v1/chat/completions"
                  (llm::llama-chat-url ":9931"))
         (string= "http://localhost:9931/v1/chat/completions"
                  (llm::llama-chat-url "localhost:9931"))
         (string= "http://box:9931/v1/chat/completions"
                  (llm::llama-chat-url "http://box:9931"))
         (string= "http://box:9931/custom"
                  (llm::llama-chat-url "http://box:9931/custom"))))

  (check "a bare --llama is an error, not the default address"
    (handler-case (progn (llm::llama-chat-url "") nil)
      (error () t)))

  (check "a port must be colon-prefixed — a bare number is an error"
    (and (handler-case (progn (llm::llama-chat-url "9931") nil)
           (error () t))
         (handler-case (progn (llm::llama-chat-url "localhost:") nil)
           (error () t))))

  (check "the port is read out of the chat URL, IPv6 brackets and all"
    (and (string= "9931" (llm::%url-port "http://localhost:9931/v1/chat/completions"))
         (string= "8080" (llm::%url-port "http://127.0.0.1:8080/v1/chat/completions"))
         (string= "9931" (llm::%url-port "http://[::1]:9931/v1/chat/completions"))
         (string= "443" (llm::%url-port "https://api.example.com:443/v1/chat/completions"))
         (null (llm::%url-port "http://localhost/v1/chat/completions"))))

  (check "the local label names the endpoint and the model that answered"
    (flet ((label (url model)
             (let ((llm:*llm-url* url) (llm:*llm-model* model))
               (llm:local-model-label))))
      (and (string= "local:9931:deepseek-v4-flash"
                    (label "http://localhost:9931/v1/chat/completions" "deepseek-v4-flash"))
           ;; Either half may be missing; neither is invented.
           (string= "local:9931" (label "http://localhost:9931/v1/chat/completions" nil))
           (string= "local:deepseek-v4-flash"
                    (label "http://localhost/v1/chat/completions" "deepseek-v4-flash"))
           (string= "local" (label "http://localhost/v1/chat/completions" nil))))))

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&operandi engine empty-turn oracle~%")
  (run-llama-picks)

  ;; A normal single answer passes straight through.
  (check "normal answer returns verbatim"
    (string= "HELLO" (final (assistant-resp :content "HELLO"))))

  ;; A null-content final turn must NOT leak as the answer.
  (check "a single null turn never returns \"NULL\"/empty"
    (let ((r (final (assistant-resp :content 'null))))
      (and (stringp r) (plusp (length r))
           (not (string-equal r "NULL")) (not (string= r "")))))

  ;; Stall then recover: two empty turns get nudged, then a real answer.
  (check "nudges past empty turns, then returns the real answer"
    (string= "DONE"
             (final (assistant-resp :content 'null)
                    (assistant-resp :content 'null)
                    (assistant-resp :content "DONE"))))

  ;; Never-recovers: after the nudge budget, salvage the last real content
  ;; (here, the output of the one tool call it did make).
  (check "salvages the last tool result when the model only stalls"
    (let ((r (final (assistant-resp :tool-calls (vector (bash-call "c1" "echo SALVAGEME")))
                    (assistant-resp :content 'null)
                    (assistant-resp :content 'null)
                    (assistant-resp :content 'null)
                    (assistant-resp :content 'null))))
      (and (stringp r) (search "SALVAGEME" r))))

  ;; The empty counter resets on a productive turn — a null between two
  ;; tool calls shouldn't count toward the give-up budget.
  (check "empty counter resets after a tool call"
    (string= "FINISHED"
             (final (assistant-resp :tool-calls (vector (bash-call "c1" "echo one")))
                    (assistant-resp :content 'null)
                    (assistant-resp :tool-calls (vector (bash-call "c2" "echo two")))
                    (assistant-resp :content 'null)
                    (assistant-resp :content "FINISHED"))))

  ;; A turn CUT OFF at the output cap after writing some text is not a final
  ;; answer: the text is usually the lead-in to a tool call that the cap
  ;; swallowed ("Now the test files.", "First the header:").  Twice in one
  ;; afternoon a worker ended its whole run on exactly that.
  (check "text cut off at the output cap is not taken as the answer"
    (string= "DONE"
             (final (assistant-resp :content "Recon done. First the header:"
                                    :finish "length")
                    (assistant-resp :content "DONE"))))

  ;; run returns a usage struct (calls/tokens/cost) as its 4th value.
  (check "run returns a usage struct as its 4th value"
    (progn
      (install-mock (list (assistant-resp :content "HI")))
      (multiple-value-bind (text history iters usage) (eng:run "task" :verbose nil)
        (declare (ignore text history iters))
        (and (typep usage 'llm:usage) (>= (llm:usage-calls usage) 1)))))

  ;; msg-text normalizer: null symbol / blank -> NIL; real string kept.
  (check "msg-text normalizes null and blank to NIL"
    (and (null (operandi.engine::msg-text (llm:ht "content" 'null)))
         (null (operandi.engine::msg-text (llm:ht "content" "   ")))
         (string= "x" (operandi.engine::msg-text (llm:ht "content" "x")))))

  ;; --- OpenRouter 200-with-error-body (exhausted grant / refusal) ---
  ;; The regression: a refused turn came back BLANK while a usage.cost echoed in
  ;; the body got billed on our side even though OpenRouter charged nothing.
  (flet ((error-body (msg code &optional cost)
           (let ((e (llm:ht "error" (llm:ht "message" msg "code" code))))
             (when cost (setf (gethash "usage" e) (llm:ht "cost" cost "prompt_tokens" 12)))
             e)))
    (check "an error-body turn surfaces the provider reason, not a blank"
      (let ((r (final (error-body "Insufficient credits" 402))))
        (and (stringp r) (search "Insufficient credits" r))))

    (check "an error-body turn charges NOTHING (no fabricated cost)"
      (progn
        (install-mock (list (error-body "Insufficient credits" 402 0.0031d0)))
        (multiple-value-bind (text history iters usage) (eng:run "task" :verbose nil)
          (declare (ignore text history iters))
          (and (typep usage 'llm:usage)
               (zerop (llm:usage-cost-usd usage))     ; the echoed cost is NOT counted
               (zerop (llm:usage-calls usage))))))     ; a refused call isn't a real call

    (check "a REAL turn still counts its reported cost (guard didn't over-fire)"
      (let ((resp (assistant-resp :content "OK")))
        (setf (gethash "usage" resp) (llm:ht "cost" 0.0020d0 "prompt_tokens" 5))
        (install-mock (list resp))
        (multiple-value-bind (text history iters usage) (eng:run "task" :verbose nil)
          (declare (ignore text history iters))
          (and (typep usage 'llm:usage)
               (> (llm:usage-cost-usd usage) 0.0d0)
               (>= (llm:usage-calls usage) 1))))))

  ;; The 0.000¢ regression: PPQ (like OpenRouter/Routstr) reports TOKENS
  ;; unprompted but `cost` ONLY when the request asks with
  ;; {"usage":{"include":true}}.  The engine used to exclude :PPQ from that
  ;; request, so every turn folded a cost of 0.  Assert the field is present
  ;; for the backends that need it, and absent for :TINFOIL (whose enclave
  ;; dialect rejects the unknown field).
  (check "the request asks for cost on :ppq (the 0.000¢ fix)"
    (let ((llm:*llm-backend* :ppq) (llm:*llm-model* "deepseek/deepseek-v4.1-flash"))
      (let ((body (eng::build-chat-body
                   (vector (llm:ht "role" "user" "content" "hi")) nil 5 0.0 nil)))
        (let ((u (gethash "usage" body)))
          (and (hash-table-p u) (eq t (gethash "include" u)))))))

  (check "the request asks for cost on :openrouter and :routstr too"
    (every (lambda (b)
             (let ((llm:*llm-backend* b) (llm:*llm-model* "m"))
               (let ((u (gethash "usage" (eng::build-chat-body
                                          (vector (llm:ht "role" "user" "content" "hi"))
                                          nil 5 0.0 nil))))
                 (and (hash-table-p u) (eq t (gethash "include" u))))))
           '(:openrouter :routstr)))

  (check "the request does NOT send usage.include to :tinfoil"
    (let ((llm:*llm-backend* :tinfoil) (llm:*llm-model* "gpt-oss-120b"))
      (null (gethash "usage" (eng::build-chat-body
                              (vector (llm:ht "role" "user" "content" "hi"))
                              nil 5 0.0 nil)))))

  ;; The sealed Path A enclave reports tokens but NO cost, so a confidential
  ;; run would still show 0.000¢.  fold-usage prices such a turn from PPQ's
  ;; catalogue instead of folding a 0.
  (check "a costless :ppq turn is priced from the catalogue, not folded as 0"
    (let ((llm:*llm-backend* :ppq) (llm:*llm-model* "private/gpt-oss-120b")
          (operandi.ppq:*ppq-pricing* '(("private/gpt-oss-120b"
                                         :in 1.0d0 :out 2.0d0 :cache-read 0.1d0)))
          (operandi.ppq::*ppq-pricing-fetched-p* t)
          (u (llm:make-usage)))
      (llm:fold-usage u (llm:ht "usage" (llm:ht "prompt_tokens" 1000
                                                "completion_tokens" 500)))
      ;; 1000 * $1/1M + 500 * $2/1M = 0.001 + 0.001 = 0.002
      (and (> (llm:usage-cost-usd u) 0.0d0)
           (< (abs (- (llm:usage-cost-usd u) 0.002d0)) 1d-9))))

  (check "a provider-reported cost is NOT overridden by the catalogue"
    (let ((llm:*llm-backend* :ppq) (llm:*llm-model* "private/gpt-oss-120b")
          (operandi.ppq:*ppq-pricing* '(("private/gpt-oss-120b"
                                         :in 1.0d0 :out 2.0d0 :cache-read 0.1d0)))
          (operandi.ppq::*ppq-pricing-fetched-p* t)
          (u (llm:make-usage)))
      (llm:fold-usage u (llm:ht "usage" (llm:ht "prompt_tokens" 1000
                                                "completion_tokens" 500
                                                "cost" 0.5d0)))
      ;; the reported cost stands, times PPQ's measured charge multiple
      ;; (*PPQ-FEE-FACTOR*): it is passed through, never replaced by the catalogue
      (< (abs (- (llm:usage-cost-usd u) (* 0.5d0 operandi.llm::*ppq-fee-factor*))) 1d-9)))

  (check "an unknown model with no reported cost folds 0, not an error"
    (let ((llm:*llm-backend* :ppq) (llm:*llm-model* "no/such-model")
          (operandi.ppq:*ppq-pricing* nil)
          (operandi.ppq::*ppq-pricing-fetched-p* t)
          (u (llm:make-usage)))
      (llm:fold-usage u (llm:ht "usage" (llm:ht "prompt_tokens" 10
                                                "completion_tokens" 5)))
      (zerop (llm:usage-cost-usd u))))

  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (zerop *fail*))

(sb-ext:exit :code (if (run) 0 1))
