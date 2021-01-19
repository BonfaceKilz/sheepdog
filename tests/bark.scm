;;; (tests-bark) --- sheepdog module for Guile.
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

(define-module (tests-bark)
  #:use-module (sheepdog)
  #:use-module (sheepdog bark)
 #:use-module (srfi srfi-64))


(test-begin "test-bark")

(test-equal "Run job as list"
  "test\n"
  (run-job '("echo" "test")))

(test-equal "Run job as string"
  "test\n"
  (run-job "echo test"))

(test-equal "Run job as procedure"
  "test\n"
  (run-job (λ ()
             "test\n")))

(test-end "test-bark")
