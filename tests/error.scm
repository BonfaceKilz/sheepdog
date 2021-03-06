;;; (tests-error) --- sheepdog module for Guile.
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

(define-module (tests-error)
  #:use-module (sheepdog)
  #:use-module (sheepdog error)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64))

(test-begin "test-error")

;;; Check 'sheepdog-error' error code return value.
(test-equal "sheepdog-error: exit code"
  42
  (match (primitive-fork)
    (0                                  ;child
     (sheepdog-error 42 "exit with 42"))
    ((= waitpid (pid . exit-code))      ;parent
     (status:exit-val exit-code))))

;;; Check 'sheepdog-error' output with basic error code.
(test-equal "sheepdog-error: output"
  "sheepdog: token"
  (call-with-output-string
    (λ (port)
      (match (pipe)
        ((in . out)
         (match (primitive-fork)
           (0                           ;child
            (close in)
            (with-error-to-port out
              (λ () (sheepdog-error 37 "token"))))
           ((= waitpid (pid . exit-code)) ;parent
            (close out)
            (display (read-line in) port))))))))


;;; Check sheepdog-error output when error code is 0.
(test-equal "sheepdog-error: output no-exit"
  "sheepdog: foobar\n"
  (call-with-output-string
    (λ (port)
      (with-error-to-port port
        (λ ()
          (sheepdog-error 0 "foo" "bar"))))))

;;; Check that sheepdog-error doesn't print anything on the standard output.
(test-equal "sheepdog-error: only stderr"
  ""
  (with-output-to-string
    (λ () (sheepdog-error 0 "foo" "bar"))))

(test-end "test-error")
