(define-module (krisb config machines features vcs)
  #:use-module (guix gexp)
  #:use-module (gnu packages version-control)
  #:use-module (gnu home services)
  #:use-module (krisb config common)
  #:export (feature-git
            feature-jujutsu))


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


;;;
;;; Jujutsu
;;;
;; Distributed VCS (more ergonomic than Git!)

(define (feature-jujutsu)
  (list (simple-service 'jujutsu-package
            home-profile-service-type
          (list (@ (abbe packages rust) jujutsu)))
        (simple-service 'jujutsu-config-files
            home-xdg-configuration-files-service-type
          `(("jj/config.toml"
             ,(local-file (config-files-path "jujutsu/config.toml")))
            ;; Compatibility with the fish shell
            ("fish/functions/fish_jj_prompt.fish"
             ,(local-file (config-files-path "jujutsu/fish_jj_prompt.fish")))
            ("fish/functions/fish_vcs_prompt.fish"
             ,(local-file (config-files-path "jujutsu/fish_vcs_prompt.fish")))))))
