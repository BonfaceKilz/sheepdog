(define-module (tests-bark)
  #:use-module (sheepdog)
  #:use-module (sheepdog bark)
  #:use-module (srfi srfi-64))

(test-begin "test-suite")

(test-equal "Run Job"
  "test"
  (run-job "echo test"))

(test-end "test-suite")

