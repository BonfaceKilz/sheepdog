;;; guix.scm --- sheepdog's guix bootstrapping file.
;;;
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;;
;;; This file is part of sheepdog.
;;;
;;; sheepdog is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; sheepdog is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with sheepdog. If not, see https://www.gnu.org/licenses/.

(use-modules
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix download)
  (guix git-download)
  (guix build-system gnu)
  (gnu packages)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages mail)
  (gnu packages pkg-config)
  (gnu packages texinfo))

(define-public my-guile-8sync
  (package
    (inherit guile-8sync)
    (name "my-guile-8sync")
    (version "0.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.savannah.gnu.org/git/8sync.git")
                    (commit "5074289a33640bb3bd78a711d7ceb645d7ae0cfd")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f71vgv338illn0bx1kibcd7j2nn0civy1jz2yqbjjnbivx0g63i"))))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("guile" ,guile-3.0)
                     ("pkg-config" ,pkg-config)
                     ("texinfo" ,texinfo)))))

(define my-guile-redis
  (package
    (inherit guile-redis)
    (name "my-guile-redis")
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aconchillo/guile-redis")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zk2x37lw6ygf7rwy71svnsian8lj51axpxmm66ah7dazn69swlm"))))))

(package
  (name "guile-sheepdog")
  (version "0.1")
  (source "./guile-sheepdog-0.1.tar.gz")
  (build-system gnu-build-system)
  (arguments `())
  (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)
      ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-3.0)))
  (propagated-inputs `(("guile-redis" ,my-guile-redis)
                       ("guile-readline" ,guile-readline)
                       ("guile-8sync" ,my-guile-8sync)
                       ("mailutils" ,mailutils)
                       ("guile-colorized" ,guile-colorized)))
  (synopsis "")
  (description "")
  (home-page "")
  (license license:gpl3+))

