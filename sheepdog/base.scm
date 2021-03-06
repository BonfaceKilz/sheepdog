;;; (sheepdog base) --- sheepdog module for Guile.
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

(define-module (sheepdog base)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:export (<cmd-pipe*>
            <cmdline-options>
            make-cmdline-options
            pipe-pair))

(define-record-type <cmdline-options>
  (make-cmdline-options always cmd channel tag host port)
  cmd-line-options?
  (always  cmdline-options-name)
  (cmd     cmdline-options-cmd)
  (channel cmdline-options-channel)
  (tag     cmdline-options-tag)
  (host    cmdline-options-host)
  (port    cmdline-options-port))

(define-record-type <cmd-pipe*>
  (make-cmd-pipe* pid stdin stdout stderr)
  cmd-pipe*?
  (pid    cmd-pipe*-pid)
  (stdin  cmd-pipe*-stdin)
  (stdout cmd-pipe*-stdout)
  (stderr cmd-pipe*-stderr))

;; See:
;; https://wiki.gnu.tools/git/gnu-tools-wiki/tree/code/modules/email.scm
(define (pipe-pair command)
  "Run COMMAND as a separate process and return a <cmd-pipe*> record type which
contain's the aforementioned process' pid, stdin, stdout, and stderr."
  (let ((input (pipe))
        (output (pipe))
        (output/error (pipe)))
    (match (primitive-fork)
      (0 ;; Inside child-process
       (dynamic-wind
         (const #t)
         (λ ()
           (match-let (((input-in . input-out) input)
                       ((output-in . output-out) output)
                       ((err-in . err-out) output/error))
             (close-port input-out)
             (close-port output-in)
             (close-port err-in)
             (dup2 (fileno input-in) 0)
             (dup2 (fileno output-out) 1)
             (dup2 (fileno err-out) 2))
           (apply execlp (car command) command))
         (λ ()
           (primitive-_exit 127))))
      (pid
       (match-let (((input-in . input-out) input)
                   ((output-in . output-out) output)
                   ((err-in . err-out) output/error))
         (close-port input-in)
         (close-port output-out)
         (close-port err-out)
         ;; output-in is the command's STDIN; input-out is the commands
         ;; STDOUT; and err-in is the command's STDERR
         (make-cmd-pipe* pid output-in input-out err-in))))))
