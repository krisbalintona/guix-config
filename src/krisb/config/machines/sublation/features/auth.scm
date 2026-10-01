(define-module (krisb config machines sublation features auth)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services gnupg)
  #:export (feature-gnupg))

(define (feature-gnupg)
  (list (simple-service 'gnupg-package
            home-profile-service-type
          (list gnupg))
        (service home-gpg-agent-service-type)))
