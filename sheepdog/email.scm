;;; (sheepdog email) --- sheepdog module for Guile.
;;;
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;;
;;; This file is part of sheepdog.
;;;
;;; sheepdog is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; sheepdog is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with sheepdog. If not, see https://www.gnu.org/licenses/.

;;; Borrowed from:
;;; https://wiki.gnu.tools/git/gnu-tools-wiki/tree/code/modules/email.scm

(define-module (sheepdog email)
  #:use-module (sheepdog base)
  #:use-module (ice-9 match)
  #:use-module (mailutils mailutils)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-19)
  #:use-module ((web uri) #:select (uri-encode))
  #:export (date->rfc822-string
            compose-message
            send-email
            send-email*)
  #:re-export (mu-debug))

(define (date->rfc822-string date)
  "Return a date string like \"Thu, 13 Feb 2020 18:09:31 +0100\" for use in a
'Date' header."
  (define days
    #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
  (define months
    #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov"
      "Dec"))

  ;; Return locale-independent day/month names.
  (define (day-name date)
    (vector-ref days (date-week-day date)))
  (define (month-name date)
    (vector-ref months (- (date-month date) 1)))

  (date->string date
                (string-append (day-name date) ", ~e "
                               (month-name date) " ~Y ~H:~M:~S ~z")))

(define* (insert-newlines str #:optional (line-length 76))
  "Insert newlines in STR every LINE-LENGTH characters."
  (let loop ((result '())
             (str str))
    (if (string-null? str)
        (string-concatenate-reverse result)
        (let* ((length (min (string-length str) line-length))
               (prefix (string-take str length))
               (suffix (string-drop str length)))
          (loop (cons (string-append prefix "\n") result)
                suffix)))))

(define (dump-port/convert-newlines input output)
  "Dump INPUT to OUTPUT, converting '\n' to '\n\r'."
  (let loop ()
    (match (get-u8 input)
      ((? eof-object?) #t)
      (10
       (put-bytevector output #vu8(13 10))
       (loop))
      (octet
       (put-u8 output octet)
       (loop)))))

(define* (attach-file! mime data #:key
                       (attachment (mu-message-create))
                       (file-mime-type "application/octet-stream")
                       (binary-file? #t)
                       (inline-file? #f))
  "Attach FILE to MIME, an object returned by 'mu-mime-create'."
  (let ((port (mu-message-get-port attachment "w")))
    (put-bytevector port
                    (if binary-file?
                        (string->utf8
                         (insert-newlines (base64-encode data)))
                        data))
    (close-port port)
    (when binary-file?
      (mu-message-set-header attachment
                             "Content-Transfer-Encoding"
                             "base64"))
    (mu-message-set-header attachment
                           "Content-Type" file-mime-type)
    (when inline-file?
      (mu-message-set-header attachment "Content-Disposition" "inline"))
    (mu-mime-add-part mime attachment)))

(define* (compute-message-id message domain #:optional seed)
  "Return a message ID string."
  (string-append "<" (number->string (object-address message) 16)
                 "." (number->string
                      (or seed
                          (string-hash (or (mu-message-get-header message "Subject")
                                           "")))
                      16)
                 "@" domain ">"))

(define* (compose-message from to
                          #:key reply-to text subject file
                          (domain "")
                          (date (time-utc->date (current-time time-utc)))
                          (file-mime-type "image/jpeg")
                          user-agent
                          (binary-file? #t)
                          (inline-file? #t))
  "Compose a message, and return a message object."
  (let* ((mime    (mu-mime-create))
         (message (mu-message-create))
         (body    (mu-message-get-port message "w")))
    (mu-message-set-header message
                           "Content-Type"
                           "text/plain; charset=utf-8")
    (put-bytevector body (string->utf8 text))
    (newline body)
    (close-port body)
    (mu-mime-add-part mime message)
    (when file
      (attach-file! mime
                    (call-with-input-file file get-bytevector-all)
                    #:file-mime-type file-mime-type
                    #:binary-file? binary-file?
                    #:inline-file? inline-file?))    
    (let ((result (mu-mime-get-message mime)))
      (mu-message-set-header result "From" from)
      (mu-message-set-header result "To" to)
      (mu-message-set-header result "Date" (date->rfc822-string date))
      (mu-message-set-header result "Message-ID"
                             (compute-message-id message domain
                                                 (and=> text string-hash)))
      (when subject
        (mu-message-set-header result "Subject" subject))
      (when reply-to
        (mu-message-set-header result "Reply-To" reply-to))
      (when user-agent
        (mu-message-set-header result "User-Agent" user-agent))
      result)))

(define (set-multipart/signed-content-type! message)
  (let ((content-type (mu-message-get-header message "Content-Type"))
        (mixed        "multipart/mixed; "))
    (when (string-prefix? mixed content-type)
      (mu-message-set-header message "Content-Type"
                             (string-append
                              "multipart/signed; "
                              (string-drop content-type
                                           (string-length mixed))
                              "; micalg=pgp-sha256; "
                              "protocol=\"application/pgp-signature\"")
                             #t))))

(define* (send-email message uri)
  "Send MESSAGE returned by 'compose-message', using the mailer URI
e.g from smtp that would like: smtp://user:pass@host:port"
  (mu-register-format)  ;; Registers all available formats
  (mu-message-send message uri))

(define (send-email* message from to subject uri)
  "Send MESSAGE given the right fields."
  (send-email (compose-message from
                               to
                               #:reply-to from
                               #:text message
                               #:subject subject)
              uri))
