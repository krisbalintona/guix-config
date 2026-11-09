(define-module (krisb config machines Maur features auth)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services ssh)
  #:export (feature-openssh))

(define (feature-openssh)
  (list (service home-openssh-service-type
          (home-openssh-configuration
            (add-keys-to-agent "yes")
            (hosts
             (list
              (openssh-host (name "sublation")
                            (host-name "sublation.netbird.selfhosted")
                            (user "krisbalintona")
                            (identity-file "~/.ssh/private-machines")
                            (forward-agent? #t))
              (openssh-host
                (name "codeberg.org")
                (user "git")
                (identity-file "~/.ssh/git-forges"))))))))
