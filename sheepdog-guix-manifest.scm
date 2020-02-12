;; ~/opt/guix/bin/guix package --manifest=sheepdog-guix-manifest.scm -p ~/opt/sheepdog
;; . ~/opt/sheepdog/etc/profile
(use-package-modules guile guile-xyz emacs emacs-xyz)

(packages->manifest
 (list
       glibc-utf8-locales
       emacs
       emacs-rainbow-delimiters
       emacs-magit
       emacs-rainbow-mode
       emacs-geiser
       emacs-json-mode
       emacs-markdown-mode
       emacs-org
       emacs-paredit
       guile-2.2
       guile-sjson
       guile-redis
       guile-sqlite3
       ))
