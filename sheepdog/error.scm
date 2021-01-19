;;; (sheepdog error) --- sheepdog module for Guile.
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

(define-module (sheepdog error)
  #:use-module (ice-9 rdelim)
  #:export (sheepdog-error
            catch-all))


(define (sheepdog-error exit-code . rest)
  "Print an error message (made up from the parts of REST), and if the
EXIT-CODE error is fatal (present and non-zero) then exit to the system with
EXIT-CODE."
  (with-output-to-port (current-error-port)
    (lambda ()
      (for-each display (cons "sheepdog: " rest))
      (newline)))
  (when (and exit-code (not (eq? exit-code 0)))
    (primitive-exit exit-code)))

(define (catch-all thunk)
  (with-exception-handler
    (lambda (exn)
      (format (current-error-port)
              "Uncaught exception: ~s\n" exn)
      #f)
    thunk
    #:unwind? #t))
