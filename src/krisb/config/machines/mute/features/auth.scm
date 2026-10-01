(define-module (krisb config machines mute features auth)
  #:use-module (guix gexp)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services gnupg)
  #:export (feature-gnupg))

(define (feature-gnupg)
  (list (simple-service 'gnupg-packages
            home-profile-service-type
          (list gnupg pinentry))
        (service home-gpg-agent-service-type
          (home-gpg-agent-configuration
            (pinentry-program
             (file-append pinentry "/bin/pinentry"))))))
