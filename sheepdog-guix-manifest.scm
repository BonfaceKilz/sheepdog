;; ~/opt/guix/bin/guix package --manifest=sheepdog-guix-manifest.scm -p ~/opt/sheepdog
;; . ~/opt/sheepdog/etc/profile
(use-package-modules databases guile guile-xyz emacs emacs-xyz)

(packages->manifest
 (list
       ;; ---- Runtime
       glibc-utf8-locales
       redis
       guile
       guile-sjson
       guile-redis
       ;; ---- Development
       emacs
       emacs-rainbow-delimiters
       emacs-magit
       emacs-rainbow-mode
       emacs-geiser
       emacs-json-mode
       emacs-markdown-mode
       emacs-org
       emacs-paredit
       ))
