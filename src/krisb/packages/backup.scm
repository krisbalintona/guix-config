(define-module (krisb packages backup)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (nonguix build-system binary))

(define-public kopia-bin
  (package
    (name "kopia-bin")
    (version "0.22.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/kopia/kopia/releases/download/v"
                       version
                       "/kopia-0.22.3-linux-x64.tar.gz"))
       (sha256 (base32 "0b8cg764a512rmi39gpswi4iwr1kxv393akwrbfkmirzh7h0dsfr"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     `(#:strip-binaries? #f
       #:install-plan '(("kopia" "bin/"))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs source #:allow-other-keys)
                      (invoke "tar" "--strip-components=1" "-xf" source))))))
    (home-page "https://kopia.io/")
    (synopsis "Backup and restore tool")
    (description " Cross-platform backup tool with fast, incremental backups, client-side end-to-end encryption, compression and data deduplication.")
    (license license:asl2.0)))
