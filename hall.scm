(hall-description
  (name "sheepdog")
  (prefix "guile")
  (version "0.1")
  (author "Bonface Munyoki")
  (copyright (2020))
  (synopsis "")
  (description "")
  (home-page "")
  (license gpl3+)
  (dependencies `())
  (files (libraries
           ((scheme-file "sheepdog")
            (directory
              "sheepdog"
              ((directory
                 "monitor"
                 ((compiled-scheme-file "monitor-error")
                  (scheme-file "monitor-error")))
               (scheme-file "error")
               (compiled-scheme-file "bark")
               (scheme-file "base")
               (scheme-file "bark")))))
         (tests ((directory
                   "tests"
                   ((log-file "bark")
                    (test-result-file "bark")
                    (scheme-file "error")
                    (scheme-file "bark")))))
         (programs ((directory "scripts" ())))
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (text-file "HACKING")
            (text-file "COPYING")
            (directory
              "doc"
              ((texi-file "sheepdog")
               (text-file "stamp-vti")
               (info-file "guile-sheepdog")
               (text-file ".dirstamp")
               (texi-file "version")))
            (text-file "NEWS")
            (text-file "AUTHORS")
            (text-file "ChangeLog")
            (text-file "NEWS")
            (text-file "AUTHORS")
            (text-file "ChangeLog")
            (text-file "NEWS")
            (text-file "AUTHORS")
            (text-file "ChangeLog")
            (text-file "NEWS")
            (text-file "AUTHORS")
            (text-file "ChangeLog")))
         (infrastructure
           ((scheme-file "guix")
            (scheme-file "hall")
            (directory
              "build-aux"
              ((text-file "mdate-sh")
               (text-file "install-sh")
               (text-file "missing")
               (scheme-file "test-driver")
               (tex-file "texinfo")))
            (autoconf-file "configure")
            (automake-file "Makefile")
            (in-file "pre-inst-env")
            (directory
              "build-aux"
              ((text-file "mdate-sh")
               (text-file "install-sh")
               (text-file "missing")
               (scheme-file "test-driver")
               (tex-file "texinfo")))
            (autoconf-file "configure")
            (automake-file "Makefile")
            (in-file "pre-inst-env")
            (directory
              "build-aux"
              ((text-file "mdate-sh")
               (text-file "install-sh")
               (text-file "missing")
               (scheme-file "test-driver")
               (tex-file "texinfo")))
            (autoconf-file "configure")
            (automake-file "Makefile")
            (in-file "pre-inst-env")
            (directory
              "build-aux"
              ((text-file "mdate-sh")
               (text-file "install-sh")
               (text-file "missing")
               (scheme-file "test-driver")
               (tex-file "texinfo")))
            (autoconf-file "configure")
            (automake-file "Makefile")
            (in-file "pre-inst-env")))))
