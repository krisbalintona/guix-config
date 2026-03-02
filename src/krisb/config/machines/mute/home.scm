(define-module (krisb config machines mute home)
  #:use-module (krisb config common)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu system shadow)      ; For user-group 
  #:use-module (gnu home)               ; provides home-environment
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages gnupg)                    ; For pinentry
  #:use-module (gnu home services gnupg)
  #:use-module (sops secrets)
  #:use-module (sops home services sops)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services backup)
  #:use-module (krisb packages fonts)
  #:use-module (gnu packages wordnet)
  #:use-module (gnu services dict)
  #:use-module (gnu home services dict)
  )



(define-public mute-home-environment
  (home-environment
    (packages
     (append
      common-home-packages
      (specifications->packages
       (list
        "nss-certs"
        "glibc-locales"
        "xmodmap"
        "font-iosevka"
        "font-google-noto-emoji" ; For emojis
        "font-iosevka"
        "font-iosevka-aile-nerd-font"
        "font-iosevka-term-ss04-nerd-font"
        "font-iosevka-ss11"
        "font-iosevka-ss11-nerd-font"
        "font-iosevka-term-ss11-nerd-font"
        "font-overpass-nerd-font"
        "font-jetbrains-mono-nerd-font"
        "nano"
        "neovim"
        "zotero"
        "mpv"
        "libreoffice"
        "wl-clipboard"
        "emacs-nerd-icons"
        "emacs-lsp-booster"
        "texinfo"                               ; Compiling Info files
        "pdftk"                                 ; For pdf-meta-edit
        "dico"
        "gcide"
        "wordnet"
        "enchant"
        "aspell" "aspell-dict-en"
        "hunspell" "hunspell-dict-en" "hunspell-dict-en-us"
        "nuspell"
        "vale"
        "python-proselint"
        "notmuch"
        "python-lieer"
        "l2md"
        ))))

    (services
     (append
      common-home-services
      (cons*
       (service home-bash-service-type
         (home-bash-configuration
           (aliases '(("grep" . "grep --color=auto")
                      ("la" . "ls -la")
                      ("ll" . "ls -l")
                      ("ls" . "ls -p --color=auto")))))
       (service home-fish-service-type
         (home-fish-configuration
           (config
            (list (plain-file "fish_greeting.fish" "set -g fish_greeting")))))
       (simple-service 'fish-fisher
           ;; Install fisher if it isn't already installed, then symlink
           ;; fish_plugins, then update plugins.  We do this altogether to
           ;; ensure the operations are done in sequence.
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
             (system "fish -c \"fisher update\"")))
       (service home-gpg-agent-service-type
         (home-gpg-agent-configuration
           (pinentry-program
            (file-append pinentry "/bin/pinentry"))))
       (service home-sops-secrets-service-type
         (home-sops-service-configuration
           (secrets
            (list
             (sops-secret
               (key '(".authinfo"))
               (file sops-mute-secrets-file)
               (permissions #o400))
             (sops-secret
               (key '("gmi-credentials" "personal"))
               (file sops-mute-secrets-file)
               (permissions #o400))
             (sops-secret
               (key '("gmi-credentials" "uni"))
               (file sops-mute-secrets-file)
               (permissions #o400))))))
       (service home-openssh-service-type
         (home-openssh-configuration
           (add-keys-to-agent "yes")
           (hosts
            (list
             (openssh-host (name "gitlab.com")
                           (user "PreciousPudding")
                           (identity-file "~/.ssh/id_ed25519"))
             (openssh-host (name "github.com")
                           (user "krisbalintona")
                           (identity-file "~/.ssh/id_ed25519"))
             (openssh-host (name "sublation")
                           ;; Hostname of my remote machine, resolved
                           ;; via local DNS
                           (host-name "sublation.home.arpa")
                           (user "krisbalintona")
                           (identity-file "~/.ssh/id_ed25519")
                           (forward-agent? #t))
             (openssh-host (name "sublation-backup")
                           (host-name "sublation.home.arpa")
                           (user "krisbalintona")
                           (identity-file "~/.ssh/id_ed25519-sublation_backups")
                           (forward-agent? #t))))))
       (simple-service 'home-bash-keychain
           home-bash-service-type
         (home-bash-extension
           (bash-profile (list (local-file (config-files-path "bash/keychain.bash") "keychain.bash")))))
       (simple-service 'home-restic-emacs-repos
           home-restic-backup-service-type
         (list
          (restic-job/defaults
           #:name "restic-emacs-repos"
           #:repository sops-mute-repository-path
           #:schedule "0 * * * *"
           #:files (list (string-append (getenv "HOME") "/emacs-repos")))))
       (simple-service 'home-restic-emails
           home-restic-backup-service-type
         (list
          (restic-job/defaults
           #:name "restic-emails"
           #:repository sops-mute-repository-path
           #:schedule "30 2 * * *"
           #:files (list (string-append (getenv "HOME") "/Documents/emails")))))
       (simple-service 'authinfo-file-service-type
           home-activation-service-type
         #~(begin
             (use-modules (guix build utils))
             (let* ((source (string-append "/run/user/" (number->string (getuid)) "/secrets/.authinfo"))
                    (target (string-append (getenv "HOME") "/.authinfo")))
               (format #t "Directly symlinking .authinfo (~a) to ~a~%" source target)
               (when (false-if-exception (lstat target))
                 (delete-file target))
               (symlink source target))))
       (service home-dicod-service-type
         (for-home
          (dicod-configuration
            (handlers (list
                       (dicod-handler
                        (name "wordnet")
                        (module "wordnet")
                        (options
                         (list #~(string-append "wnhome=" #$wordnet))))))
            (databases (list
                        (dicod-database
                         (name "wordnet")
                         (complex? #t)
                         (handler "wordnet"))
                        %dicod-database:gcide)))))
       (simple-service 'enchant-file-service-type
           home-xdg-configuration-files-service-type
         `(("enchant/enchant.ordering"
            ,(local-file (config-files-path "enchant/enchant.ordering")))))
       (simple-service 'vale-config-file-service-type
           home-xdg-configuration-files-service-type
         `(("vale/.vale.ini"
            ,(local-file (config-files-path "vale/vale.ini")))))
       (simple-service 'vale-styles-service-type
           home-files-service-type
         `((".local/share/vale/styles/krisb-custom"
            ,(local-file (config-files-path "vale/krisb-custom") #:recursive? #t))))
       (simple-service 'notmuch-config-files-service-type
           home-xdg-configuration-files-service-type
         `(("notmuch/default"
            ,(local-file (config-files-path "notmuch") #:recursive? #t))))
       (simple-service 'notmuch-maintain
           home-shepherd-service-type
         (list
          (shepherd-timer
              '(notmuch-compact)
            ;; Run every Friday at 9pm
            "0 21 * * 5"
            #~("notmuch" "compact")
            #:documentation "Compact notmuch mail database weekly.")))
       ;; Syncing mailing lists with lore2maildir (l2md)
       (simple-service 'l2md-sync-mailing-lists
           home-shepherd-service-type
         (list
          (shepherd-timer '(l2md-sync-mailing-lists)
            ;; Run every 30 minutes.  Cannot run too frequently,
            ;; otherwise risk an 504 timeout error when trying to
            ;; fetch emails.
            "*/30 * * * *"
            #~("sh" "-c"
               #$(string-join '("echo 'Starting l2md fetch...'" "&&"
                                "l2md" "&&"
                                "echo 'Done fetching!'" "&&"
                                "echo 'Starting notmuch new...'" "&&"
                                "notmuch" "new" "&&"
                                "echo 'All done!'"))))))
       (simple-service 'l2md-files-service-type
           home-xdg-configuration-files-service-type
         `(("l2md/config"
            ,(local-file (config-files-path "l2md/config")))))
       ;; Syncing gmail emails with lieer
       (simple-service 'gmi-sync
           home-shepherd-service-type
         (let ((personal-email-dir "Documents/emails/personal/")
               (uni-email-dir "Documents/emails/personal/")
               ;; For gmi sync, use --verbose to have the most useful
               ;; log messages, and use --resume such that in the case
               ;; of a sudden power off, the .lock file will not cause
               ;; a lock up.  Of course, this may lead to the local
               ;; changes to be ignored until the next sync, but that
               ;; is preferable to my mail not being synced
               ;; unknowingly.
               (gmi-sync "gmi sync --verbose")
               ;; This restores the gmi internal state if gmi sync
               ;; fails; this is like a “last resort.”
               (gmi-rescue "cp .state.gmailieer.json{.bak,}")
               ;; Show as much as possible in the logs.
               (notmuch-new "notmuch new --verbose"))
           (list
            (shepherd-timer '(gmi-sync-personal)
              "*/4 * * * *" ; Run every 4 minutes
              #~("sh" "-c"
                 ;; Run from $HOME
                 #$(string-join
                    (list "cd " personal-email-dir "&&"
                          "echo 'Starting sync...'" "&&"
                          "(" gmi-sync "||"
                          "{" gmi-rescue "&&"
                          gmi-sync ";" "}" ")" "&&"
                          "echo 'Done with sync!'" "&&"
                          "echo 'Starting notmuch new...'" "&&"
                          notmuch-new "&&"
                          "echo 'All done!'"))))
            (shepherd-timer '(gmi-sync-uni)
              "*/4 * * * *" ; Run every 4 minutes
              ;; Run from $HOME
              #~("sh" "-c"
                 #$(string-join
                    (list "cd " uni-email-dir "&&"
                          "echo 'Starting sync...'" "&&"
                          "(" gmi-sync "||"
                          "{" gmi-rescue "&&"
                          gmi-sync ";" "}" ")" "&&"
                          "echo 'Done with sync!'" "&&"
                          "echo 'Starting notmuch new...'" "&&"
                          notmuch-new "&&"
                          "echo 'All done!'")))))))
       ;; REVIEW 2025-07-02: Don't remember if needed on Guix system or
       ;; just a foreign distro.
       ;; Certificates
       (simple-service 'krisb-ssl-certs ; Requires nss-certs package
           home-environment-variables-service-type
         ;; NOTE: We install nss-certs via guix home, so SSL_CERT_DIR
         ;; is relative to ~/.guix-home/.  If nss-certs is installed
         ;; via e.g. guix install, then it would be relative to
         ;; ~/.guix-profile/.
         '(("SSL_CERT_DIR" . "$HOME/.guix-home/profile/etc/ssl/certs")
           ("SSL_CERT_FILE" . "$SSL_CERT_DIR/ca-certificates.crt")
           ("GIT_SSL_CAINFO" . "$SSL_CERT_FILE")
           ("CURL_CA_BUNDLE" . "$SSL_CERT_FILE")))
       ;; WSL2-specific
       (simple-service 'krisb-wslg-display-service-type
           home-environment-variables-service-type
         '(("DISPLAY" . ":0")))
       ;; Guix on a foreign distro
       (simple-service 'krisb-foreign-distro
           home-environment-variables-service-type
         '(("GUIX_PROFILE" . "$HOME/.guix-profile")))
       ;; Base services
       %base-home-services)))))
