(define-module (sheepdog bark)
  #:use-module (sheepdog error)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:export (run-job))


; See:
; https://wiki.gnu.tools/git/gnu-tools-wiki/tree/code/modules/email.scm
(define (pipe-pair command)
  "Run COMMAND as a separate process and return four values values: its PID,
an output port to write on COMMAND's standard input, an input port to
read COMMAND's standard output, and an output/error port to read
COMMAND's standard error."
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
         (values pid input-out output-in err-in))))))


(define (run-command action)
  "Run ACTION using pipe-pair function."
  (define command
    (if (list? action)
        action
        (string-split action #\space)))
  (receive (pid output input err)
      (pipe-pair command)
    (close-port output)
    (let ((results (get-bytevector-all input))
          (err-msg (get-bytevector-all err)))
      (close-port err)
      (close-port input)
      (match (waitpid pid)
        ((_ . 0) (utf8->string results)) ;; success
        ((_ . status)
         (throw 'sheepdog-error status (utf8->string err-msg)))))))


(define (run-job action)
  (catch-all
   (λ ()
     (cond
      ((procedure? action) (action))
      ((or (list? action) (string? action))
       (run-command action))
      (else
       throw 'sheepdog-error 2
       "job: invalid args (action: should be a lambda"
       "function, string or list)")))))
