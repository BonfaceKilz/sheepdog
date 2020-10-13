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
