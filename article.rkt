#lang racket/base

(provide
  (struct-out article)
  list-all-article-ids
  load-article
  sanitize-article-id
  save-article
  new-article)

(require
  racket/port
  racket/string
  markdown/parse)

(struct article (id title content raw))

(define (markdown-file? path)
  (regexp-match? #rx"\\.md$" path))

(define (articles-dir root)
  (build-path root "articles"))

(define (sanitize-article-id id)
  (and (regexp-match-exact? #rx"[A-Za-z0-9_-]+" id)
       id))

(define (make-article-path root id)
  (and id
       (non-empty-string? id)
       (build-path (articles-dir root)
                   (string-append id ".md"))))

(define (list-all-article-ids root)
  (for/list ([path (directory-list (articles-dir root))]
             #:when (markdown-file? path))
    (regexp-replace #rx"\\.md$" (path->string path) "")))

(define (find-article-title content)
  (define h1 (assq 'h1 content))
  (and h1 (caddr h1)))

(define (load-article root id)
  (define path (make-article-path root id))
  (and
    path
    (with-handlers ([exn:fail:filesystem:errno? (lambda (err) #f)])
      (define raw (port->string (open-input-file path) #:close? #t))
      (define content (parse-markdown raw))
      (article
        id
        (or (find-article-title content) "No Title")
        content
        raw))))

(define (save-article root id content)
  (define path (make-article-path root id))
  (if path
      (with-handlers ([exn:fail:filesystem:errno?
                       (lambda (err) "/!\\ Unable to save file!")])
        (with-output-to-file path
          (lambda ()
            (write-string content)
            "File saved successfuly!")
          #:exists 'replace))
      "/!\\ Unable to save file!"))

(define (new-article id)
  (article
    id
    "New Article"
    '((p "This article does not exist yet.")
      (p "Create it by clicking the edit link."))
    ""))
