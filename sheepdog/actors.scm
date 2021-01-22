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
  #:use-module (redis main)
  #:use-module (8sync)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:export (run-monitor))

(define-class <worker> (<actor>)
  (host #:init-keyword #:host
        #:init-value "127.0.0.1"
        #:getter redis-host)
  (port #:init-keyword #:port
        #:init-value 6379
        #:getter redis-port)
  (channels #:init-keyword #:channels
            #:init-value '("sheepdog")
            #:getter redis-channels)
  (procedure #:init-keyword #:procedure
             #:init-value (lambda (x) x)
             #:getter get-procedure)
  (actions #:allocation #:each-subclass
           #:init-thunk (build-actions
                         (*init* worker-get-message))))

(define (worker-get-message worker message)
  (define conn (redis-connect #:host (redis-host worker)
                              #:port (redis-port worker)))
  (redis-subscribe conn (redis-channels worker))
  (while (actor-alive? worker)
    ((get-procedure worker) (redis-subscribe-read conn))))


(define* (run-monitor #:key (host "127.0.0.1")
                      (port 6379)
                      (channels '("sheepdog"))
                      (procedure (lambda (x) x)))
  (let* ((hive (make-hive))
         (worker (bootstrap-actor hive <worker>
                                  #:host host
                                  #:port port
                                  #:channels channels
                                  #:procedure procedure)))
    (run-hive hive '())))
