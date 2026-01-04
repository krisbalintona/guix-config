(define-module (krisb services shells)
  #:use-module (guix gexp)
  #:use-module (guix records)
  
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  
  #:use-module (rosenthal packages rust-apps) ; For Atuin package

  #:export (home-atuin-configuration
            home-atuin-service-type))

(define-configuration/no-serialization home-atuin-configuration
  (atuin
   (file-like atuin)
   "The Atuin package to use.")
  (atuin-bash-flags
   (list-of-strings '())
   "Extra flags passed to `atuin init bash`.")
  (atuin-fish-flags
   (list-of-strings '())
   "Extra flags passed to `atuin init fish`."))

(define %home-atuin-bash
  (match-record-lambda <home-atuin-configuration>
      (atuin atuin-bash-flags)
    (home-bash-extension
      (bashrc
       (list (mixed-text-file "atuin.bash"
               "eval \"$("
               atuin "/bin/atuin init bash "
               (string-join atuin-bash-flags " ")
               ")\"\n"))))))

(define %home-atuin-fish
  (match-record-lambda <home-atuin-configuration>
      (atuin atuin-fish-flags)
    (home-fish-extension
      (config
       (list (mixed-text-file "atuin.fish"
               atuin "/bin/atuin init fish "
               (string-join atuin-fish-flags " ")
               " | source\n"))))))

(define %home-atuin-profile
  (match-record-lambda <home-atuin-configuration>
      (atuin)
    (list atuin)))

(define home-atuin-service-type
  (service-type
    (name 'atuin)
    (extensions
     (list
      ;; Bash integration
      (service-extension home-bash-service-type
                         %home-atuin-bash)
      ;; Fish integration
      (service-extension home-fish-service-type
                         %home-atuin-fish)
      ;; Add Atuin to the home profile, too
      (service-extension home-profile-service-type
                         %home-atuin-profile)))
    (default-value
      (home-atuin-configuration))
    (description
     "Enable Atuin integration for Fish and Bash, and install Atuin.")))
