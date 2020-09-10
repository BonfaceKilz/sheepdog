(define-module (sheepdog bark)
  #:use-module (sheepdog error)
  #:use-module ((ice-9 popen))
  #:use-module (ice-9 rdelim)
  #:export (run-job))

(define (run-job job)
  (let* ((port (open-input-pipe job))
         (str (read-line port)))
    (close-pipe port)
    str))

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
