(define-module (krisb config machines features vcs)
  #:use-module (guix gexp)
  #:use-module (gnu packages version-control)
  #:use-module (gnu home services)
  #:use-module (krisb config common)
  #:export (feature-git))


;;;
;;; Git
;;;

(define (feature-git)
  (list (simple-service 'git-package
            home-profile-service-type
          (list git))
        (simple-service 'git-config-files
            home-xdg-configuration-files-service-type
          `(("git/config" ,(local-file (config-files-path "git/config")))))))
