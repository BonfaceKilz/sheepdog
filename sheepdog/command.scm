;;; (sheepdog command) --- sheepdog module for Guile.
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

(define-module (sheepdog command)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:export (args-fold*
            pipe-pair)
  #:export-syntax (define-command)
  #:re-export (option))

(define (args-fold* args options unrecognized-option-proc operand-proc . seeds)
  "A wrapper on top of `args-fold' that does proper user-facing error
reporting."
  (catch 'misc-error
    (lambda ()
      (apply args-fold args options unrecognized-option-proc
             operand-proc seeds))
    (lambda (key proc msg args . rest)
      (match args
        ((or (#\c) ("config"))
         (display "Please provide a configuration file!\n"))
        ((flag)
         (display (apply (list format #f "Invalid argument: ~a~%\n" flag)))))
      (exit 1))))

(define-syntax define-command
  (syntax-rules (synopsis)
    "Define the given command as a procedure along with its synopsis.
The synopsis becomes the docstring of the procedure."
    ((_ (name . args)
        (synopsis doc) body ...)
     (define (name args)
       doc
       body ...))))

