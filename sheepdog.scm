(define-module (sheepdog))

;; The composite module the re-exports everything from the public modules.
(eval-when (eval load compile)
  (begin
    (define %public-modules
      '(bark base error))

    (for-each (let ((i (module-public-interface (current-module))))
                (lambda (m)
                  (module-use! i (resolve-interface `(sheepdog ,m)))))
              %public-modules)))
