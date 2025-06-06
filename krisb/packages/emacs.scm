(define-module (krisb packages emacs)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module ((emacs-master) :prefix emacs-master:))

(define-public emacs-master-custom
  (package
    (inherit emacs-master:emacs-master)
    (name "emacs-master-custom")
    (source
     ;; I inherit from the original package’s origin but replace the
     ;; commit (and its associated hash)
     (origin
       (inherit (package-source emacs-master:emacs-master))
       (uri (git-reference
             (url "https://github.com/emacs-mirror/emacs.git")
             (commit "eb788fd8fd2026fa4d29b918ff95b12d8e3e0bab")))
       (sha256 (base32 "1bpsbyqs58p474qfrxhydxmnyabi79bzl10k7rds4xl0wbfx3ivg"))))
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-master:emacs-master)
       ((#:configure-flags flags)
        ;; I manipulate the build flags here.  I can add, remove, or
        ;; replace flags that are already declared in the package this
        ;; inherits from.
        `(delete-duplicates (append '("--with-pgtk"
                                      "--with-x-widgets")
                                    ,flags)
                            equal?))))))

(define-public emacs-master-igc-custom
  (package
    (inherit emacs-master:emacs-master-igc)
    (name "emacs-master-igc-custom")
    ;; I inherit from the original package’s origin but replace the
    ;; commit (and its associated hash)
    (source
     (origin
       (inherit (package-source emacs-master:emacs-master-igc))
       (uri (git-reference
             (url "https://github.com/krisbalintona/emacs.git")
             (commit "e70e3cf5e9855a2fb55d7128d601bbca0e6e0119")))
       (sha256 (base32 "113h525s2510mxrjrmbz3db0f5qp2203i5s0yq7k839fcizz4ri4"))))
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-master:emacs-master-igc)
       ((#:configure-flags flags)
        ;; I manipulate the build flags here.  I can add, remove, or
        ;; replace flags that are already declared in the package this
        ;; inherits from.
        `(delete-duplicates (append '("--with-x-widgets")
                                    ,flags)
                            equal?))))))
