(define-module (sheepdog bark)
  #:use-module (sheepdog error)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-9)
  #:export (run-job))


(define-record-type <cmdline-options>
  (make-cmdline-options always cmd channel tag host port)
  cmd-line-options?
  (always  cmdline-options-name)
  (cmd     cmdline-options-cmd)
  (channel cmdline-options-channel)
  (tag     cmdline-options-tag)
  (host    cmdline-options-tag)
  (port    cmdline-options-port))

(define-record-type <pipe*>
  (make-pipe* pid stdin stdout stderr)
  pipe*?
  (pid    pipe*-pid)
  (stdin  pipe*-stdin)
  (stdout pipe*-stdout)
  (stderr pipe*-stderr))

; See:
; https://wiki.gnu.tools/git/gnu-tools-wiki/tree/code/modules/email.scm
(define (pipe-pair command)
  "Run COMMAND as a separate process and return a <pipe*> record type which
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
         (make-pipe* pid output-in input-out err-in))))))


(define (run-command action)
  "Run ACTION using pipe-pair function."
  (define command
    (if (list? action)
        action
        (string-split action #\space)))
  (match-let ((($ <pipe*> pid stdin stdout stderr) (pipe-pair command)))
    (close-port stdout)
    (let ((results (get-bytevector-all stdin))
          (err-msg (get-bytevector-all stderr)))
      (close-port stderr)
      (close-port stdin)
      (match (waitpid pid)
        ((_ . 0)
         (utf8->string results)) ;; success
        ((_ . status)
         (throw 'sheepdog-error status (utf8->string err-msg)))))))


(define (run-job action)
  (catch-all
   (λ ()
     (match action
       ((? procedure?) (action))
       ((or (? list?) (? string?))
        (run-command action))
       (_
        (throw 'sheepdog-error 2
               "job: invalid args (action: should be a lambda"
               "function, string or list)"))))))
