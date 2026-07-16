;;; src/search.lisp  (operandi.search)
;;;
;;; Brave Search API wrapper — backs the WebSearch tool. Query for
;;; entities/facts, get back a small list of recent web results (and
;;; news, videos, etc when available).
;;;
;;; The token is NOT in this file. It lives at:
;;;   ~/.operandi/brave-search.token
;;; (or wherever *TOKEN-FILE* points). Read once on first call,
;;; cached. NEVER commit the token to the repo.
;;;
;;; API docs: https://api.search.brave.com/app/documentation/web-search/get-started

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (ql:quickload '(:dexador :com.inuoe.jzon :cl-ppcre) :silent t))

(defpackage #:operandi.search
  (:use #:cl)
  (:local-nicknames (#:dex   #:dexador)
                    (#:jzon  #:com.inuoe.jzon)
                    (#:ppcre #:cl-ppcre))
  (:export #:*token-file*
           #:*api-base*
           #:search-web
           #:search-news
           #:results-as-articles))

(in-package #:operandi.search)

(defparameter *token-file*
  (merge-pathnames ".operandi/brave-search.token" (user-homedir-pathname))
  "Where to read the Brave Search API token from. Format: a single
   line, the bare token. Mode 600.")

(defparameter *api-base* "https://api.search.brave.com/res/v1")

(defvar *cached-token* nil)

(defun read-token ()
  (or *cached-token*
      (setf *cached-token*
            (handler-case
                (with-open-file (s *token-file*)
                  (string-trim '(#\Space #\Newline #\Return #\Tab)
                               (read-line s nil "")))
              (error () nil)))))

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
         (dex:get (concatenate 'string *api-base* path)
                  :force-string t
                  :keep-alive nil
                  :read-timeout 10
                  :connect-timeout 5
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
  (let ((bytes (sb-ext:string-to-octets s :external-format :utf-8)))
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

(defun search-web (q &key (count 5) (freshness "pw"))
  "Web search. FRESHNESS one of NIL, 'pd' (past day), 'pw' (past
   week), 'pm' (past month), 'py' (past year), 'p1d', etc.
   Returns a list of plists per result:
     (:title :url :description :age)"
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

(defun search-news (q &key (count 5) (freshness "pw"))
  "News-vertical search. Same args/return shape as SEARCH-WEB."
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
