;;; src/search.lisp  (operandi.search)
;;;
;;; Web search behind the WebSearch tool, with three backends behind one
;;; SEARCH-WEB. Which one runs follows *BACKEND* (OPERANDI_SEARCH), or when
;;; that is NIL, the LLM backend:
;;;
;;;   :openrouter — OpenRouter's web plugin. A throwaway 1-token completion
;;;                 with plugins=[{id:"web"}]; the results come back as
;;;                 url_citation annotations (title, url, a content
;;;                 snippet). Same credential as the model, nothing else to
;;;                 manage; ~$4 per 1000 results.
;;;   :searxng    — a self-hosted SearXNG (Docker, localhost) via its JSON
;;;                 API. No token at all — the fit for the local-llama path.
;;;                 Needs `search.formats: [html, json]` in its settings.
;;;   :brave      — Brave Search API. Token from BRAVE_API_KEY, else
;;;                 ~/.operandi/brave-search.token (a bare token on one line,
;;;                 mode 600). NEVER commit it.
;;;
;;; Auto picks :openrouter when the model runs there, else :searxng if one
;;; answers at *SEARXNG-URL*, else :brave if a token is present.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  ;; #+QUICKLISP: the .asd already loads these; this is for loading the file by hand.  Guarded
  ;; because READING `ql:' is an error in an image without Quicklisp, before anything runs.
  #+quicklisp (ql:quickload '(:com.inuoe.jzon :cl-ppcre :babel) :silent t))

(defpackage #:operandi.search
  (:use #:cl)
  (:local-nicknames (#:http  #:operandi.http)
                    (#:jzon  #:com.inuoe.jzon)
                    (#:ppcre #:cl-ppcre)
                    (#:llm   #:operandi.llm))
  (:export #:*backend* #:*searxng-url* #:*token-file* #:*api-base*
           #:effective-backend #:backend-status #:parse-backend
           #:search-web
           #:search-news
           #:results-as-articles))

(in-package #:operandi.search)

(defun parse-backend (s)
  "\"openrouter\"|\"searxng\"|\"brave\" -> keyword; \"auto\"/\"\" -> NIL.
   Second value NIL if S wasn't one of those."
  (let ((k (and (stringp s) (string-downcase (string-trim " " s)))))
    (cond ((member k '("openrouter" "or") :test #'string=) (values :openrouter t))
          ((string= k "searxng") (values :searxng t))
          ((string= k "brave") (values :brave t))
          ((member k '("auto" "") :test #'string=) (values nil t))
          (t (values nil nil)))))

(defparameter *backend*
  (let ((e (uiop:getenv "OPERANDI_SEARCH"))) (and e (parse-backend e)))
  "Forced search backend, or NIL to follow the LLM backend (see file header).
   Seeded from OPERANDI_SEARCH; /search in the TUI sets it.")

(defparameter *searxng-url*
  (or (uiop:getenv "SEARXNG_URL") "http://127.0.0.1:8080")
  "Base URL of a SearXNG instance. The sandbox profile allows localhost:8080.")

(defparameter *token-file*
  (merge-pathnames ".operandi/brave-search.token" (user-homedir-pathname))
  "Where to read the Brave Search API token from. Format: a single
   line, the bare token. Mode 600.")

(defparameter *api-base* "https://api.search.brave.com/res/v1")

(defvar *cached-token* nil)

(defun read-token ()
  "The Brave token: BRAVE_API_KEY if set, else the token file; cached."
  (or *cached-token*
      (setf *cached-token*
            (let ((env (uiop:getenv "BRAVE_API_KEY")))
              (if (and env (plusp (length env)))
                  env
                  (handler-case
                      (with-open-file (s *token-file*)
                        (string-trim '(#\Space #\Newline #\Return #\Tab)
                                     (read-line s nil "")))
                    (error () nil)))))))

(defvar *searxng-probe* :unknown
  "Whether *SEARXNG-URL* answered, cached for the process: :UNKNOWN, T, NIL.")

(defun searxng-reachable-p ()
  (when (eq *searxng-probe* :unknown)
    (setf *searxng-probe*
          (handler-case
              (progn (http:get (format nil "~A/config" *searxng-url*) :read-timeout 2)
                     t)
            (error () nil))))
  *searxng-probe*)

(defun effective-backend ()
  "The backend a search would use right now, or NIL with a reason as the
   second value."
  (cond (*backend* *backend*)
        ((eq llm:*llm-backend* :openrouter) :openrouter)
        ((searxng-reachable-p) :searxng)
        ((read-token) :brave)
        (t (values nil (format nil "no search backend: the model isn't on OpenRouter, nothing answers at ~A, and there is no Brave token (BRAVE_API_KEY or ~A)"
                               *searxng-url* *token-file*)))))

(defun backend-status ()
  "One line for the TUI: what /search would use and why."
  (multiple-value-bind (b reason) (effective-backend)
    (format nil "~A~A"
            (if b (string-downcase (symbol-name b)) "none")
            (cond (reason (format nil " — ~A" reason))
                  (*backend* " (forced; OPERANDI_SEARCH or /search)")
                  (t (format nil " (auto: ~A)"
                             (case b
                               (:openrouter "the model runs on OpenRouter")
                               (:searxng (format nil "SearXNG answers at ~A" *searxng-url*))
                               (:brave "Brave token present"))))))))

(defun strip-html (s)
  (when s
    (let* ((s (or (ppcre:regex-replace-all "<[^>]+>" s "") s))
           (s (or (ppcre:regex-replace-all "&amp;" s "&") s))
           (s (or (ppcre:regex-replace-all "&quot;" s "\"") s))
           (s (or (ppcre:regex-replace-all "&lt;" s "<") s))
           (s (or (ppcre:regex-replace-all "&gt;" s ">") s))
           (s (or (ppcre:regex-replace-all "&#39;" s "'") s)))
      (string-trim '(#\Space #\Tab #\Newline #\Return) s))))

(defun api-get (path)
  "Raw GET against the API. Returns parsed JSON hash, or NIL on error."
  (let ((token (read-token)))
    (unless token
      (format *error-output* "~&brave: no token at ~A~%" *token-file*)
      (return-from api-get nil))
    (handler-case
        (jzon:parse
         (http:get (concatenate 'string *api-base* path)
                   :read-timeout 10
                   :headers `(("X-Subscription-Token" . ,token)
                             ("Accept" . "application/json"))))
      (error (e)
        (format *error-output* "~&brave api err: ~A~%" e)
        nil))))

(defun url-encode (s)
  "URL-encode a string for use in query parameters. Multi-byte chars
   (e.g. 'ñ', 'é', '北') are emitted as their UTF-8 byte sequence with
   each byte percent-encoded — Brave (and most servers) reject single-byte
   percent-encodings of codepoints > 127."
  (let ((bytes (babel:string-to-octets s :encoding :utf-8)))
    (with-output-to-string (out)
      (loop for b across bytes do
            (let ((c (code-char b)))
              (cond ((or (and (< b 128) (alphanumericp c))
                         (find c "-_.~"))
                     (write-char c out))
                    ((= b (char-code #\Space))
                     (write-char #\+ out))
                    (t
                     (format out "%~2,'0X" b))))))))

(defun brave-search-web (q &key (count 5) (freshness "pw"))
  (let* ((path (format nil "/web/search?q=~A&count=~A&search_lang=en&country=US~A"
                       (url-encode q) count
                       (if freshness (format nil "&freshness=~A" freshness) "")))
         (res (api-get path)))
    (when res
      (let ((web (gethash "web" res)))
        (when web
          (let ((items (gethash "results" web)))
            (when (vectorp items)
              (loop for r across items
                    collect (list :title (strip-html (gethash "title" r))
                                  :url   (gethash "url" r)
                                  :description (strip-html
                                                (gethash "description" r))
                                  :age   (gethash "age" r))))))))))

;;; ---- OpenRouter web plugin -------------------------------------------

(defun openrouter-search-web (q &key (count 5) (freshness "pw"))
  "One cheap completion with the web plugin on; the results are the
   url_citation annotations. FRESHNESS has no API knob here, so it goes
   into the query text as a hint."
  (let* ((hint (cond ((null freshness) "")
                     ((string= freshness "pd") " (past day)")
                     ((string= freshness "pw") " (past week)")
                     ((string= freshness "pm") " (past month)")
                     ((string= freshness "py") " (past year)")
                     (t (format nil " (~A)" freshness))))
         (body (llm:ht "model" llm:*llm-model*
                       "max_tokens" 1
                       "reasoning" (llm:ht "enabled" nil)
                       "plugins" (vector (llm:ht "id" "web" "max_results" (max 1 (min count 10))))
                       "messages" (vector (llm:ht "role" "user"
                                                  "content" (format nil "Web search: ~A~A" q hint)))))
         (res (handler-case
                  (jzon:parse
                   (http:post llm:*llm-url*
                              :headers `(("Authorization" . ,(format nil "Bearer ~A" llm:*llm-auth-token*))
                                         ("Content-Type" . "application/json"))
                              :content (jzon:stringify body)
                              :read-timeout 30))
                (error (e)
                  (format *error-output* "~&openrouter search err: ~A~%" e)
                  nil)))
         (choices (and res (gethash "choices" res)))
         (msg (and (vectorp choices) (plusp (length choices))
                   (gethash "message" (aref choices 0))))
         (anns (and msg (gethash "annotations" msg))))
    (when (vectorp anns)
      (loop for a across anns
            for u = (and (hash-table-p a) (gethash "url_citation" a))
            when (hash-table-p u)
              collect (list :title (strip-html (gethash "title" u))
                            :url (gethash "url" u)
                            :description (let ((c (ppcre:regex-replace-all "\\s+" (or (strip-html (gethash "content" u)) "") " ")))
                                           (if (> (length c) 600) (subseq c 0 600) c))
                            :age nil)))))

;;; ---- SearXNG ----------------------------------------------------------

(defun searxng-search-web (q &key (count 5) (freshness "pw"))
  (let* ((range (cond ((null freshness) nil)
                      ((string= freshness "pd") "day")
                      ((string= freshness "pw") "week")
                      ((string= freshness "pm") "month")
                      ((string= freshness "py") "year")))
         (url (format nil "~A/search?q=~A&format=json&language=en~@[&time_range=~A~]"
                      *searxng-url* (url-encode q) range))
         (res (handler-case
                  (jzon:parse (http:get url :read-timeout 15))
                (error (e)
                  (format *error-output* "~&searxng err: ~A~%" e)
                  nil)))
         (items (and res (gethash "results" res))))
    (when (vectorp items)
      (loop for r across items
            repeat count
            collect (list :title (strip-html (gethash "title" r))
                          :url (gethash "url" r)
                          :description (strip-html (gethash "content" r))
                          :age (let ((d (gethash "publishedDate" r)))
                                 (and (stringp d) d)))))))

;;; ---- dispatch -----------------------------------------------------------

(defun search-web (q &key (count 5) (freshness "pw"))
  "Web search on whichever backend is in effect (EFFECTIVE-BACKEND).
   FRESHNESS one of NIL, 'pd' (past day), 'pw' (past week), 'pm' (past
   month), 'py' (past year). Returns a list of plists per result:
     (:title :url :description :age)
   Signals an error naming the problem when no backend is available."
  (multiple-value-bind (backend reason) (effective-backend)
    (ecase backend
      (:openrouter (openrouter-search-web q :count count :freshness freshness))
      (:searxng (searxng-search-web q :count count :freshness freshness))
      (:brave (brave-search-web q :count count :freshness freshness))
      ((nil) (error "~A" reason)))))

(defun search-news (q &key (count 5) (freshness "pw"))
  "News-vertical search. Same args/return shape as SEARCH-WEB. Brave has a
   news vertical; the others get 'news' folded into the query."
  (if (eq (effective-backend) :brave)
      (brave-search-news q :count count :freshness freshness)
      (search-web (format nil "~A news" q) :count count :freshness freshness)))

(defun brave-search-news (q &key (count 5) (freshness "pw"))
  (let* ((path (format nil "/news/search?q=~A&count=~A&search_lang=en&country=US~A"
                       (url-encode q) count
                       (if freshness (format nil "&freshness=~A" freshness) "")))
         (res (api-get path)))
    (when res
      (let ((items (gethash "results" res)))
        (when (vectorp items)
          (loop for r across items
                collect (list :title (strip-html (gethash "title" r))
                              :url   (gethash "url" r)
                              :description (strip-html
                                            (gethash "description" r))
                              :age   (gethash "age" r))))))))

(defun results-as-articles (results &key (source "brave-search"))
  "Translate SEARCH-WEB / SEARCH-NEWS output into a uniform article
   plist shape: (:title :summary :url :published-at :source)."
  (loop for r in results
        collect (list :title (or (getf r :title) "")
                      :summary (or (getf r :description) "")
                      :url (getf r :url)
                      :published-at (getf r :age)
                      :source source)))
