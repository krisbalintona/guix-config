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
             (commit "8ff6e7fe58466e413b5eed22ea93869e60da8d3d")))
       (sha256 (base32 "1f8bxh1kh9z53dqjzxqgk05jv9rggdxzmxa3m63i3pwrdgvnlaaq"))))
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
             (commit "1c19182e28e90b619a4c3d5553e3b5ae03eccd12")))
       (sha256 (base32 "0dsfd3fb533yjc4bhnfsf4f7dslfp73rnb4kqnramsjyp8s1r4lx"))))
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-master:emacs-master-igc)
       ((#:configure-flags flags)
        ;; I manipulate the build flags here.  I can add, remove, or
        ;; replace flags that are already declared in the package this
        ;; inherits from.
        `(delete-duplicates (append '("--with-x-widgets")
                                    ,flags)
                            equal?))))))
