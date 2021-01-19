(define-module (sheepdog bark)
  #:use-module (sheepdog base)
  #:use-module (sheepdog error)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (redis)
  #:export (run-job))

(define* (run-command cmdline-options)
  "Run command that's wrapped inside CMDLINE-OPTIONS record."
  (match-let ((($ <cmdline-options> always cmd channel tag host port) cmdline-options))
    (define command
      (if (list? cmd)
          cmd
          (string-split cmd #\space)))
    (match-let ((($ <pipe*> pid stdin stdout stderr) (pipe-pair command)))
      (close-port stdout)
      (let ((results (get-bytevector-all stdin))
            (err-msg (get-bytevector-all stderr)))
        (close-port stderr)
        (close-port stdin)
        (define conn (redis-connect #:host host #:port port))
        (match (waitpid pid)
          ((_ . 0)
           (utf8->string results)) ;; success
          (( . status)
           (redis-publish conn
                          (string-append channel tag)
                          (format #f "~a- ~a"
                                  (strftime "%c" (localtime (current-time)))
                                  (match err-msg
                                    ((? bytevector?) (utf8->string err-msg))
                                    (_ err-msg))))))))))

(define (load-config config-path)
  "Load configuration options from CONFIG-PATH"
  (define conf '())
  (with-input-from-file config-path
    (lambda ()
      (let loop ((line (read)))
        (when (not (eof-object? line))
          (set! conf (append conf (list line)))
          (loop (read))))))
  conf)

(define* (run-job action
                  #:key
                  (always #f)
                  (channel "sheepdog")
                  (tag "")
                  (host "127.0.0.1")
                  (port 6379))
  "Run ACTION and if an an error occurs, send the error to Redis."
  (catch-all
   (λ ()
     (match action
       ((? procedure?) (action))
       ((or (? list?) (? string?))
        (run-command (make-cmdline-options always action channel tag host port)))
       (_
        (throw 'sheepdog-error 2
               "job: invalid args (action: should be a lambda"
               "function, string or list)"))))))
