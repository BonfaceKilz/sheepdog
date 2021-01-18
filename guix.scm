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
  (gnu packages pkg-config)
  (gnu packages texinfo))

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
                       ("guile-colorized" ,guile-colorized)))
  (synopsis "")
  (description "")
  (home-page "")
  (license license:gpl3+))

