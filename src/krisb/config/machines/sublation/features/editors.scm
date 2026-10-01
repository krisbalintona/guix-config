(define-module (krisb config machines sublation features editors)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:export (feature-editors))

(define (feature-editors)
  (list (simple-service 'editor-packages
            home-profile-service-type
          (list nano
                vim
                neovim
                emacs))))
