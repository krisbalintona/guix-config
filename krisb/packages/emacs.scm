(define-module (krisb packages emacs)
  #:use-module (guix packages)
  #:use-module ((emacs-master) :prefix emacs-master:)
  ;; #:use-module (gnu packages emacs)
  #:use-module (guix utils))

(define-public emacs-master-custom
  (package
    (inherit emacs-master:emacs-master)
    (name "emacs-master-custom")
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
