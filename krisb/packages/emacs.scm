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
             (commit "b469506030a7a0d4027922a72d8ed3355fac5163")))
       (sha256 (base32 "02r5a5asj8v30fywm41l8q9wkv9yvklmxfm2g6kd09zkkwlbr92i"))))
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-master:emacs-master)
       ((#:configure-flags flags)
        ;; I manipulate the build flags here.  I can add, remove, or
        ;; replace flags that are already declared in the package this
        ;; inherits from.
        `(delete-duplicates (append '("--with-x-toolkit=gtk3"
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
