;; Taken from
;; https://git.sr.ht/~puercopop/glue/tree/default/item/src/glue/packages/atuin.scm

(define-module (krisb packages atuin)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((gnu packages base)
                #:select (glibc))
  #:use-module ((gnu packages gcc) #:select (gcc))
  #:use-module ((gnu packages sqlite) #:select (sqlite))
  #:use-module ((nonguix build-system binary) #:select (binary-build-system)))

(define-public atuin-bin
  (package
   (name "atuin-bin")
   ;; Use revision helper?
   (version "18.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/atuinsh/atuin/releases/download/v"
                         version
                         "/atuin-x86_64-unknown-linux-gnu.tar.gz"))
     (sha256 (base32 "14hp673i8in9adahg01bldlwyip7kg5vdnqi5jczinv8ibxnswg3"))))
   (supported-systems '("x86_64-linux"))
   (build-system binary-build-system)
   (inputs `(("gcc:lib" ,gcc "lib")
             ("glibc" ,glibc)))
   (arguments
    `(#:strip-binaries? #f
      #:install-plan '(("atuin-x86_64-unknown-linux-gnu/atuin" "bin/"))
      #:patchelf-plan `(("atuin-x86_64-unknown-linux-gnu/atuin" ("glibc" "gcc:lib")))
      #:phases (modify-phases %standard-phases
                              (replace 'unpack
                                       (lambda* (#:key inputs source #:allow-other-keys)
                                         (invoke "tar" "-zxf" source))))))
   (home-page "https://atuin.sh/")
   (synopsis "Sync, search and backup shell history")
   (description "Atuin lets you sync, search and backup shell history. It stores your shell
history in an SQLite database.")
   (license license:expat)))
