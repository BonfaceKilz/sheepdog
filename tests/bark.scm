(define-module (tests-bark)
  #:use-module (sheepdog)
  #:use-module (sheepdog bark)
  #:use-module (srfi srfi-64))


(test-begin "test-bark")

(test-equal "Run job as list"
  "test\n"
  (run-job '("echo" "test")))

(test-equal "Run job as string"
  "test\n"
  (run-job "echo test"))

(test-equal "Run job as procedure"
  "test\n"
  (run-job (λ ()
             "test\n")))

(test-end "test-bark")
