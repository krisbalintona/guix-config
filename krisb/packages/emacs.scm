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
             (commit "492adf20b91520e96fb198e6e936e3145359c43b")))
       (sha256 (base32 "06varjkp65k7m0kkf3gq2ykig3hwxmainncl0lx6ihz923ymnspl"))))
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
