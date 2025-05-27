;; Based on
;; https://git.sr.ht/~puercopop/glue/tree/default/item/src/glue/packages/jujutsu.scm

(define-module (krisb packages jujutsu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages base)
  #:use-module ((nonguix build-system binary) #:select (binary-build-system)))

;; (define-public jujutsu
;;   (package
;;     (name "jujutsu")
;;     (version "0.29.0")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (string-append "https://github.com/jj-vcs/jj/archive/refs/tags/v" version ".tar.gz"))
;;        (sha256
;;         (base32 "1nczg9b5jbisi36sx79ywd40q5i4iyh3wf78sv62a88xdfh39psp"))))
;;     (build-system cargo-build-system)
;;     ;; (arguments
;;     ;;  `(#:cargo-inputs
;;     ;;    (("openssl" ,openssl))
;;     ;;    #:phases
;;     ;;    (modify-phases %standard-phases
;;     ;;      (add-after 'unpack 'set-openssl-env
;;     ;;          (lambda* (#:key inputs #:allow-other-keys)
;;     ;;            (setenv "OPENSSL_DIR" (assoc-ref inputs "openssl"))
;;     ;;            #t)))))
;;     (native-inputs
;;      (list pkg-config ;; cmake clang
;;         which))
;;     (inputs
;;      (list openssl))
;;     (home-page "https://martinvonz.github.io/jj/latest/")
;;     (synopsis "A Git-compatible VCS that is both simple and powerful")
;;     (description "A Git-compatible VCS that is both simple and powerful.")
;;     (license license:asl2.0)))

(define-public jujutsu-bin
  (package
   (name "jujutsu-bin")
   (version "0.29.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/martinvonz/jj/releases/download/v"
                         version "/jj-v" version "-x86_64-unknown-linux-musl.tar.gz"))
     (sha256 (base32 "18x8c5a1yj1njb4p9y3n75fq4r6pr0l96vsvzh4v3ikyyyjpqb8b"))))
   (supported-systems '("x86_64-linux"))
   (build-system binary-build-system)
   (arguments
    `(#:install-plan
      '(("jj" "bin/jj"))))
   (home-page "https://martinvonz.github.io/jj/latest/")
   (synopsis "Binary for jujuts, a Git-compatible VCS that is both simple and powerful")
   (description "Jujutsu, a Git-compatible VCS that is both simple and powerful.")
   (license license:asl2.0)))
