;;; (sheepdog actors) --- sheepdog module for Guile.
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

(define-module (sheepdog actors)
  #:use-module ((redis pubsub) #:select (redis-subscribe
                                         redis-subscribe-read))
  #:use-module ((sheepdog bark) #:select (run-job))
  #:use-module (redis)
  #:use-module (8sync)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (srfi srfi-71)
  #:export (run-job-with-monitor))

(define-actor <sheepdog> (<actor>)
  ((bark bark))
  (messenger #:init-keyword #:messenger
             #:getter get-messenger))

(define-actor <messenger> (<actor>)
  ((send-alert send-alert))
  (procedure #:init-keyword #:procedure
             #:init-value (lambda (x) x)
             #:getter get-procedure))

(define (bark sheepdog message params)
  "If an error occurs, tell the monitor actor to send a notification"
  (match params
    ((action host port channel notify?)
     ;; Start messenger
     (cond ((equal? (run-job action
                             #:host host
                             #:port port
                             #:channel channel)
                    0) ;; success
            (<- (get-messenger sheepdog)
                'send-alert
                (list 'success)))
           (else
            (<- (get-messenger sheepdog)
                'send-alert (list host port channel notify?)))))))

(define (send-alert messenger message params)
  "In case of an error send a notification"
  (match params
    (('success)
     (display "\nWoof!  Sheepdog ran the command successfully!\n"))
    ((host port channel notify?)
     (when notify?
       (let ((conn (redis-connect #:host host #:port port)))
         ((get-procedure messenger) (redis-send conn
                                                (rpop `(,(format #f "queue:~a" channel)))))
         (redis-close conn))))))

(define* (run-job-with-monitor action #:key (host "127.0.0.1")
                               (port 6379)
                               (channel "sheepdog")
                               (notify? #f)
                               (procedure (lambda (x) x)))
  (let* ((hive (make-hive))
         (messenger (bootstrap-actor hive <messenger>
                                     #:procedure procedure))
         (sheepdog (bootstrap-actor hive <sheepdog>
                                    #:messenger messenger)))
    (run-hive hive
              (list
               (bootstrap-message hive
                                  sheepdog
                                  'bark
                                  (list action host port channel notify?))))))
