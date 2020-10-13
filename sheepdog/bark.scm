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
(define (pipe-pair cmd)
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
           (close-port (cdr input))
           (close-port (car output))
           (close-port (car output/error))
           (dup2 (fileno (car input)) 0)
           (dup2 (fileno (cdr output)) 1)
           (dup2 (fileno (cdr output/error)) 2)
           (apply execlp (car cmd) cmd))
         (λ ()
           (primitive-_exit 127))))
      (pid
       (close-port (car input))
       (close-port (cdr output))
       (close-port (cdr output/error))
       (values pid
               (cdr input)
               (car output)
               (car output/error))))))

(define (call-command-with-output-error-to-string cmd)
  (let* ((err-cons (pipe))
         (port (with-error-to-port (cdr err-cons)
                 (λ () (open-input-pipe cmd))))
         (_ (setvbuf (car err-cons) 'block
             (* 1024 1024 16)))
         (result (read-delimited "" port)))
    (close-port (cdr err-cons))
    (values
     result
     (read-delimited "" (car err-cons)))))

(define (job action)
  (cond ((procedure? action) action)
                      ((list? action) (λ () (primitive-eval action)))
                      ((string? action)
                       (λ ()
                         (call-command-with-output-error-to-string action)))
                      (else
                       throw 'sheepdog-error 2
                       "job: invalid args (action: should be a lambda"
                       "function, string or list)")))

(define (run-job job)
  (if (= (primitive-fork) 0)
      (dynamic-wind ;child
        (const #t)
        (λ ()
          (catch-all job))
        (λ ()
          (primitive-exit 0)))
      (begin ; parent
        (display "Done running sheepdog :)")))
