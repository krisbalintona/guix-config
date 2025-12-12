(define-module (krisb packages notmuch-tools)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages haskell-xyz) ; Supplies pandoc
  #:use-module (gnu packages mail)
  #:use-module (gnu packages ncurses))

(define-public notmuch-tools
  (package
   (name "notmuch-tools")
   (version "84c38f02a54959b0d6eaeced9dd425f420574c57")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://framagit.org/manu/notmuch-tools.git")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256 (base32 "1p6n9niaxpjsd8rdpqfbbqfbwlyb7y4wxkbgx3w6w11zw5claqrx"))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:make-flags #~(list (string-append "PREFIX=" #$output) "CC=gcc")
     #:phases
     #~(modify-phases %standard-phases
                      (delete 'configure)
                      (delete 'check))))
   (native-inputs (list pkg-config pandoc gcc))
   (inputs (list notmuch glib gmime sqlite
                 ;; 2025-05-27: Not sure why I need this, but the
                 ;; build fails without it
                 ncurses))
   (synopsis "Useful shell scripts for notmuch mail")
   (description
    "notmuch-tools is a collection of shell scripts that provide useful utilities and helpers for working with the notmuch mail indexer.")
   (home-page "https://framagit.org/manu/notmuch-tools")
   ;; Licensed under the BSD 2-Clause "Simplified" License, but that
   ;; license isn’t available in Guix?
   (license #f)))
