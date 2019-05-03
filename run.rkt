#lang racket/base

(require
  racket/cmdline
  racket/date
  web-galaxy/response
  web-galaxy/serve
  "article.rkt"
  "database.rkt")

(define (get-or-create-article id)
  (define sanitized-id (sanitize-article-id id))
  (and sanitized-id
       (or (load-article (current-server-root-path) sanitized-id)
           (new-article sanitized-id))))

(define (base-page title the-editor renderer)
  `(html ([lang "en"])
     (head
       (meta ([charset "utf-8"]))
       (title ,(format "~a - Rilouwiki" title))
       (link ([rel "icon"] [href "/favicon.ico"]))
       (link ([rel "stylesheet"] [href "/common.css"])))
     (body
       (header
         (div "Rilouwiki | "
              (a ([href "/"]) "Home")
              " | "
              (a ([href "/articles"]) "Articles")
              " | "
              "Connected as "
              (a ([href "/session"]) ,(editor-name the-editor)))
         (hr))
       (main ,(renderer))
       (footer (hr) (span "Rilouwiki v0.1 © Jérôme Martin")))))

(define (article-page the-editor the-article)
  (base-page (article-title the-article)
             the-editor
             (lambda ()
               `(article
                  ,@(article-content the-article)
                  (a ([href ,(format "/edit/~a" (article-id the-article))])
                     "Edit")))))

(define (article-list-page the-editor article-ids)
  (base-page
    "All articles"
    the-editor
    (lambda ()
      `(ul ,@(map (lambda (id)
                    (define url (format "/article/~a" id))
                    `(li (a ([href ,url]) ,url)))
                  article-ids)))))

(define (editor-page the-editor [the-article #f] [message #f])
  (base-page
    (format "Editing \"~a\"" (article-title the-article))
    the-editor
    (lambda ()
      `(form ([id "editor"]
              [method "POST"])
         (textarea ([name "content"]
                    [placeholder "Type markdown here"]
                    [cols "100"]
                    [rows "24"])
           ,(article-raw the-article))
         (a ([href ,(format "/article/~a" (article-id the-article))])
            "Back to the article")
         nbsp
         (button ([type "submit"]) "Save")
         nbsp
         ,@(if message `((span ,message)) '())))))

(define (session-page the-editor sessions current-session)
  (base-page "Your sessions" the-editor
    (lambda ()
      `(section
         (h1 "Your opened sessions")
         (table
           (tr (th "Date") (th "Expires in") (th "Current?") (th "Actions"))
           ,@(map
               (lambda (s)
                 `(tr (td ,(date->string (seconds->date (session-timestamp s)) #t))
                      (td ,(timestamp->user-friendly-string (session-timestamp s))
                      (td ,(if (eq? s current-session) "Current" ""))
                      (td (a ([href ,(format "/logout/~a" (session-id s))]) "Logout")))))
               sessions))))))

(define (error-page the-editor message)
  (base-page "Error" the-editor
    (lambda ()
      `(section (h1 "Error")
                (p ,message)))))

(define (login-page)
  '(form ([id "login"]
          [method "POST"])
     (input ([name "username"]
             [type "text"]
             [placeholder "Username"]))
     (input ([name "password"]
             [type "password"]
             [placeholder "Password"]))
     (button ([type "submit"]) "Login")))

(define (call-with-auth req func)
  (define token (req-cookie req "token"))
  (define user-id (authentified? db-conn token))
  (define the-editor (and user-id (get-editor db-conn user-id)))
  (if the-editor
      (func the-editor)
      (redirect/see-other "/login")))

(define-response (login)
  (response/page (login-page)))

(define-response (auth)
  (define username (req-data "username" req))
  (define password (req-data "password" req))
  (define the-editor (and username
                          password
                          (authentify db-conn username password)))
  (if the-editor
      (redirect/cookie "/" "token" (create-session db-conn (editor-id the-editor)))
      (response/page (login-page))))

(define-response (welcome)
  (call-with-auth req
    (lambda (the-editor)
      (define the-article (get-or-create-article "welcome"))
      (response/page (article-page the-editor the-article)))))

(define-response (article id)
  (call-with-auth req
    (lambda (the-editor)
      (define the-article (get-or-create-article id))
      (if the-article
          (response/page (article-page the-editor the-article))
          (response/not-found (error-page the-editor "Wrong article id"))))))

(define-response (article-list)
  (call-with-auth req
    (lambda (the-editor)
      (define article-ids (list-all-article-ids (current-server-root-path)))
      (response/page (article-list-page the-editor article-ids)))))

(define-response (editor id)
  (call-with-auth req
    (lambda (the-editor)
      (define the-article (get-or-create-article id))
      (if the-article
          (response/page (editor-page the-editor the-article))
          (response/not-found (error-page the-editor "Wrong article id"))))))

(define-response (save id)
  (call-with-auth req
    (lambda (the-editor)
      (define content (req-data "content" req))
      (define message (save-article (current-server-root-path) id content))
      (define the-article (get-or-create-article id))
      (if the-article
          (response/page (editor-page the-editor the-article message))
          (response/not-found (error-page the-editor "Wrong article id"))))))

(define-response (session)
  (call-with-auth req
    (lambda (the-editor)
      (define token (req-cookie req "token"))
      (define sessions (get-sessions db-conn (editor-id the-editor)))
      (define current-session (findf (lambda (s) (string=? (session-token s) token))
                                     sessions))
      (response/page (session-page the-editor sessions current-session)))))

(define-response (logout id)
  (call-with-auth req
    (lambda (the-editor)
      (delete-session db-conn (editor-id the-editor) id)
      (redirect/see-other "/session"))))

;; === start the server ===

(define static-root-path (path->string (build-path (current-server-root-path) "static")))
(define current-database-path (make-parameter "database.sqlite"))

(server-command-line
  #:once-each
  [("--database") database-path
   "Path to the SQLite database"
   (current-database-path database-path)])

(define db-conn (make-db-connection (current-database-path)))

(parameterize ([current-server-static-paths (list static-root-path)])
  (serve/all
    [GET ("") response-welcome]
    [GET ("login") response-login]
    [POST ("login") response-auth]
    [GET ("article" (string-arg)) response-article]
    [GET ("articles") response-article-list]
    [GET ("edit" (string-arg)) response-editor]
    [POST ("edit" (string-arg)) response-save]
    [GET ("session") response-session]
    [GET ("logout" (integer-arg)) response-logout]
    ))
