#lang racket/base

(provide
  (struct-out editor)
  (struct-out session)
  authentified?
  authentify
  create-session
  delete-session
  delete-sessions
  get-editor
  get-sessions
  make-db-connection
  timestamp->user-friendly-string)

(require
  racket/random
  file/sha1
  db)

(struct editor (id name))

; "CREATE TABLE editor
;   (id INTEGER PRIMARY KEY,
;    username TEXT UNIQUE,
;    password TEXT,
;    salt TEXT)"

(struct session (id token userid timestamp))

; "CREATE TABLE session
;  (id INTEGER PRIMARY KEY,
;   token TEXT,
;   userid INTEGER,
;   timestamp DATETIME)"

(define (make-db-connection path)
  (when (not (file-exists? path))
    (error 'make-db-connection "Database file could not be found: ~a" path))
  (virtual-connection
   (connection-pool
     (lambda ()
       (sqlite3-connect #:database path)))))

(define (authentified? db token)
  (and token
    (let ([db-token (query-maybe-row db "SELECT userid, timestamp FROM session WHERE token=$1" token)])
      (and db-token
        (let-values ([(userid timestamp) (vector->values db-token)])
          (and (not (expired? timestamp))
               userid))))))

(define (authentify db username password)
  (define db-user (query-maybe-row db "SELECT id, username, salt, password FROM editor WHERE username=$1" username))
  (and db-user
       (let-values ([(db-id db-name db-salt db-pass) (vector->values db-user)])
         (and (string=? db-pass
                        (hash+salt password db-salt))
              (editor db-id db-name)))))

(define (create-session db id)
  (define token (generate-unique-token))
  (query-exec db "INSERT INTO session (token, userid, timestamp) VALUES ($1, $2, $3)" token id (epoch))
  token)

(define (delete-session db userid id)
  (query-exec db "DELETE FROM session WHERE userid=$1 AND id=$2" userid id))

(define (delete-sessions db userid)
  (query-exec db "DELETE FROM session WHERE userid=$1" userid))

(define (get-editor db id)
  (define db-user (query-maybe-row db "SELECT id, username FROM editor WHERE id=$1" id))
  (and db-user
       (apply editor (vector->list db-user))))

(define (get-sessions db userid)
  (define db-sessions (query-rows db "SELECT id, token, userid, timestamp FROM session WHERE userid=$1" userid))
  (and db-sessions
       (map (lambda (row)
              (apply session (vector->list row)))
            db-sessions)))

(define (hash+salt str salt)
  (bytes->hex-string
    (sha256-bytes
      (open-input-string
        (string-append str salt)))))

(define (generate-unique-token)
  (bytes->hex-string
    (crypto-random-bytes 32)))

(define current-session-duration (make-parameter 10 integer?))

(define SECONDS-IN-A-MINUTE 60)
(define SECONDS-IN-AN-HOUR (* 60 SECONDS-IN-A-MINUTE))
(define SECONDS-IN-A-DAY (* 24 SECONDS-IN-AN-HOUR))

(define (epoch [days-offset 0])
  (+ (current-seconds)
     (* days-offset SECONDS-IN-A-DAY)))

(define (seconds-remaining timestamp)
  (define expiration-date (epoch (- (current-session-duration))))
  (- timestamp expiration-date))

(define (expired? timestamp)
  (not (positive? (seconds-remaining timestamp))))

(define (timestamp->user-friendly-string timestamp)
  (define time (seconds-remaining timestamp))
  (if (positive? time)
      (let*-values
        ([(days remains) (quotient/remainder time SECONDS-IN-A-DAY)]
         [(hours remains) (quotient/remainder remains SECONDS-IN-AN-HOUR)]
         [(minutes seconds) (quotient/remainder remains SECONDS-IN-A-MINUTE)])
        (string-append
          (if (positive? days) (format "~a days " days) "")
          (if (positive? hours) (format "~a hours " hours) "")
          (if (positive? minutes) (format "~a minutes " minutes) "")
          (if (positive? seconds) (format "~a seconds" seconds) "")))
      "Expired"))
