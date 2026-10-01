(define-module (krisb config machines features shell)
  #:use-module (guix gexp)

  #:use-module (gnu system shadow)

  #:use-module (gnu packages shells)
  #:use-module (gnu packages bash)
  #:use-module ((gnu packages guile) #:select (guile-readline))
  #:use-module ((gnu packages guile-xyz) #:select (guile-colorized))

  #:use-module (gnu services)

  #:use-module (gnu home services)
  #:use-module (gnu home services shells)

  #:use-module (krisb services shells)
  #:export (feature-base-environment
            feature-bash-shell
            feature-fish-shell))


;;;
;;; Base environment
;;;

(define (feature-base-environment)
  (list (simple-service 'base-environment-variables
            home-environment-variables-service-type
          '(("PATH" . "$HOME/.local/bin:$PATH")
            ("PAGER" . "less -RKF")))

        ;; The ~/.guile file is the configuration file for the Guile
        ;; REPL.  It runs prior to every new Guile REPL instance.  It
        ;; is the equivalent of ~/.bashrc for Guile.
        ;;
        ;; The default contents of ~/.guile (`%default-dotguile`) use
        ;; the guile-readline and guile-colorized modules then
        ;; activate their functionality when they are available.
        (simple-service 'dotguile
            home-files-service-type
          `((".guile" ,%default-dotguile)))
        ;; Have GNU Readline bindings (typical terminal bindings and
        ;; functionality) in the Guile REPL.  See the (guile) Readline
        ;; Support manual page for more information.
        ;;
        ;; This installs the package, but one needs to either
        ;; (use-modules (ice-9 readline)) followed by
        ;; (activate-readline) in a Guile/Guix REPL or do the
        ;; equivalent in their ~/.guile file (which is done above).
        (simple-service 'guile-readline-package
            home-profile-service-type
          (list guile-readline))
        ;; Colorize the output of the Guix and Guile REPL.  This
        ;; installs the package, but one needs to either (use-modules
        ;; (ice-9 colorized)) followed by (activate-colorized) in a
        ;; Guile/Guix REPL or do the equivalent in their ~/.guile file
        ;; (which is done above).
        (simple-service 'guile-colorized-package
            home-profile-service-type
          (list guile-colorized))))


;;;
;;; Bash shell
;;;

(define (feature-bash-shell)
  (list (simple-service 'bash-package
            home-profile-service-type
          (list bash-completion))
        (service home-bash-service-type
          (home-bash-configuration
            (aliases '(("grep" . "grep --color=auto")
                       ("la" . "ls -la")
                       ("ll" . "ls -l")
                       ("ls" . "ls -p --color=auto")))))))


;;;
;;; Fish shell
;;;

(define (feature-fish-shell)
  (list (service home-fish-service-type
          (home-fish-configuration
            (config
             (list
              ;; TODO 2026-09-20: Should I open a but report in Guix
              ;; about this issue?  I need to add Guix's paths into
              ;; SSH sessions because the default only adds it to
              ;; login sessions
              (plain-file "path_in_ssh.fish"
                "set -q SSH_CONNECTION; and not status is-interactive; and set -gx PATH (/bin/sh -lc 'echo $PATH' | string split :)")
              (plain-file "non_interactive_early_return.fish"
                "status is-interactive; or return")
              (plain-file "fish_greeting.fish"
                "set -g fish_greeting")))))
        (simple-service 'fish-fisher
            ;; Install fisher if it isn't already installed, then
            ;; symlink fish_plugins, then update plugins.  We do this
            ;; altogether to ensure the operations are done in
            ;; sequence.
            home-activation-service-type
          #~(begin
              (use-modules (guix build utils)
                           (krisb config common))
              (let* ((source (canonicalize-path (config-files-path "fish/fish_plugins")))
                     (target (string-append (getenv "XDG_CONFIG_HOME") "/fish/fish_plugins")))
                (format #t "Directly symlinking fish_plugins (~a) to ~a~%" source target)
                (when (false-if-exception (lstat target))
                  (delete-file target))
                (symlink source target))
              (if (not (file-exists? (string-append (getenv "XDG_CONFIG_HOME") "/fish/functions/fisher.fish")))
                  (begin
                    (format #t "Installing fisher~%")
                    (system (string-append "fish -c \"curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | "
                                           "source && fisher install jorgebucaran/fisher\""))))
              (format #t "Updating fisher plugins~%")
              (system "fish -c \"fisher update\"")))))
