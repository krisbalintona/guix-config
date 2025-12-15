;; This "home-environment" file can be passed to 'guix home reconfigure' to
;; reproduce the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to capture
;; the channels being used, as returned by "guix describe".  See the
;; "Replicating Guix" section in the manual.

(define-module (conf home-configuration)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 exceptions)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages wordnet)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (gnu services dict)
  #:use-module (gnu services backup)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services dict)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services backup)
  #:use-module (sops secrets)
  #:use-module (sops home services sops)
  #:use-module (abbe packages rust)     ; For Jujutsu
  #:use-module (krisb packages fonts)
  #:use-module (krisb packages atuin)
  #:use-module (krisb packages lieer))

(define krisb-home-environment
  (home-environment
    ;; Below is the list of packages that will show up in your Home
    ;; profile, under ~/.guix-home/profile.
    (packages (specifications->packages
               (list
                ;; Basic stuff
                "nss-certs"
                "glibc-locales"
                "man-db"
                "coreutils"
                "findutils"
                "grep"
                "sed"
                "gawk"
                "less"
                "which"
                "unzip" "zip" "gzip" "bzip2" "xz" "tar"
                "diffutils"
                "file"
                "psmisc"
                "procps"
                "inetutils"
                "net-tools"
                "wget"
                "curl"
                "tree"
                "bash-completion"
                "openssh"
                "keychain"
                "git"
                "neovim" "nano"
                "fontconfig"
                "fzf"
                "ripgrep"
                "font-iosevka"
                "xdg-utils"
                "xdg-user-dirs"
                "make"
                "cmake"
                "wl-clipboard"
                "python-lieer"
                "l2md"
                "zotero"
                "python"
                "gnupg" "pinentry"
                "mpv"
                ;; Fancy CLI tools
                "atuin-bin" ; Don't forget to log in and sync atuin on first install
                "bat"       ; Also a dependency for zoxide
                "fd"        ; Also a dependency for zoxide
                "zoxide"
                ;; Fish shell
                "grc"           ; For oh-my-fish/plugin-grc fish plugin
                ;; Emacs
                "emacs-master"
                "emacs-guix"
                "emacs-arei" "guile-next" "guile-ares-rs"
                "jujutsu"               ; From the Abbe channel
                "patch"                 ; Needed for vc-jj
                "emacs-pdf-tools"
                "enchant" "emacs-jinx"
                "aspell" "aspell-dict-en"
                "hunspell" "hunspell-dict-en" "hunspell-dict-en-us"
                "nuspell"
                "hugo"
                "texlive-latexmk"
                "emacs-lsp-booster"
                "emacs-nerd-icons"
                "notmuch" "l2md"
                "xmodmap"
                "font-google-noto-emoji" ; For emojis
                "font-iosevka"
                "font-iosevka-aile-nerd-font"
                "font-iosevka-term-ss04-nerd-font"
                "font-iosevka-ss11"
                "font-iosevka-ss11-nerd-font"
                "font-iosevka-term-ss11-nerd-font"
                "font-overpass-nerd-font"
                "font-jetbrains-mono-nerd-font"
                "texinfo"               ; To make info files
                "plocate"               ; For consult
                ;; TODO 2025-05-23: Add xargs and bash as dependencies
                ;; when I create a “notmuch” module
                "notmuch"
                "dico" "gcide" "wordnet"
                "vale" "python-proselint"
                "pdftk"                 ; For pdf-meta-edit
                ;; Other
                "libreoffice")))

    (services
     (append
      ;; Restic backups
      (list
       (service home-restic-backup-service-type
         (let ((abbe-restic (@ (abbe packages golang) restic))
               ;; Make sure the SSH key associated with
               ;; "sublation-backup" (in ~/.ssh/config) is an
               ;; authorized key on the remote (i.e., present in
               ;; ~/.ssh/authorized_keys).  Otherwise SSH attempts
               ;; will still prompt for a password (and therefore
               ;; error), even with passwordless SSH keys.
               (restic-repository "sftp:sublation-backup:/mnt/backup-hdd")
               (restic-password-file
                (string-append "/run/user/" (number->string (getuid)) "/secrets"
                               "/restic-backup-password")))
           (restic-backup-configuration
             (jobs
              (list
               (restic-backup-job
                 (restic abbe-restic)
                 (name "restic-emacs-repos")
                 (repository restic-repository)
                 (password-file restic-password-file)
                 (schedule "0 * * * *")
                 (files (list (string-append (getenv "HOME") "/emacs-repos")))
                 (extra-flags (list "--compression=max"
                                    "--pack-size=64"))
                 (verbose? #t))
               (restic-backup-job
                 (restic abbe-restic)
                 (name "restic-emails")
                 (repository restic-repository)
                 (password-file restic-password-file)
                 (schedule "30 2 * * *")
                 (files (list (string-append (getenv "HOME") "/Documents/emails")))
                 (extra-flags (list "--compression=max"
                                    "--pack-size=64"))
                 (verbose? #t))))))))
      ;; Notmuch
      (list
       (simple-service 'krisb-symlink-notmuch-config-files-service-type
           home-xdg-configuration-files-service-type
         `(("notmuch/default"
            ,(local-file "files/notmuch" #:recursive? #t))))
       (simple-service 'notmuch-maintain
           home-shepherd-service-type
         (list
          (shepherd-timer
              '(notmuch-compact)
            ;; Run every Friday at 9pm
            "0 21 * * 5"
            #~("notmuch" "compact")
            #:documentation "Compact notmuch mail database weekly."))))
      ;; Pulling emails
      (list
       ;; Syncing mailing lists with lore2maildir (l2md)
       (simple-service 'l2md-sync-mailing-lists
           home-shepherd-service-type
         (list
          (shepherd-timer '(l2md-sync-mailing-lists)
            ;; Run every 15 minutes.  Cannot run too frequently,
            ;; otherwise risk an 504 timeout error when trying to
            ;; fetch emails.
            "*/15 * * * *"
            #~("sh" "-c"
               #$(string-join '("echo 'Starting l2md fetch...'" "&&"
                                "l2md" "&&"
                                "echo 'Done fetching!'" "&&"
                                "echo 'Starting notmuch new...'" "&&"
                                "notmuch" "new" "&&"
                                "echo 'All done!'"))))))
       (simple-service 'krisb-symlink-l2md-files-service-type
           home-xdg-configuration-files-service-type
         `(("l2md/config"
            ,(local-file "files/l2md/config"))))
       ;; Syncing emails with lieer
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
                          "echo 'All done!'"))))))))
      ;; Syncthing
      (list
       (service home-syncthing-service-type
         (let* ((wsl-arch-device
                 (syncthing-device
                   (id "OQHSZRW-L2TT7IC-7USSLNU-ST7JYML-J7J6CU3-42P7NCA-WHE7BEL-SASRXA3")
                   (name "G14 2024 Arch WSL")))
                (mobile-device
                 (syncthing-device
                   (id "OVGYOBF-JPFQJKE-6CKRY7J-JULRCWK-WSGSA6Y-SQZYLLE-B2OLSDJ-6DRSTQZ")
                   (name "OnePlus 7 Pro")))
                (agenda-folder
                 (syncthing-folder
                   (id "k4vqh-rny7b")
                   (label "Agenda")
                   (path "~/Documents/org-database/agenda/")
                   (devices (list wsl-arch-device mobile-device))))
                (notes-folder
                 (syncthing-folder
                   (id "qtuzy-ufufb")
                   (label "Notes")
                   (path "~/Documents/org-database/notes")
                   (devices (list wsl-arch-device mobile-device)))))
           (for-home
            (syncthing-configuration
              (arguments (list "--no-default-folder"))
              (user "krisbalintona")
              (config-file
               (syncthing-config-file
                 ;; We use a non-standard port because we are on WSL
                 ;; with other distros and we want them using
                 ;; different ports
                 (gui-address "127.0.0.1:8386")
                 (folders (list agenda-folder notes-folder)))))))))
      ;; Dicod (dictionary server implementation)
      (list
       ;; FIXME 2025-05-24: Currently does not work, at least in WSLg
       ;; where I am writing this.  Although the non-inetd mode
       ;; version works (tested while running manually in the CLI),
       ;; inetd mode gives troubles.  For now I leave this in, since
       ;; my guesss is that it should be sufficient in non-WSLg
       ;; systems.
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
                        %dicod-database:gcide))))))
      ;; Vale
      (list
       (simple-service 'symlink-vale-config-file-service-type
           home-xdg-configuration-files-service-type
         `(("vale/.vale.ini"
            ,(local-file "files/vale/vale.ini"))))
       (simple-service 'symlink-vale-styles-service-type
           home-files-service-type
         `((".local/share/vale/styles/krisb-custom"
            ,(local-file "files/vale/krisb-custom" #:recursive? #t)))))
      ;; Enchant
      (list
       (simple-service 'krisb-symlink-enchant-files-service-type
           home-xdg-configuration-files-service-type
         `(("enchant/enchant.ordering"
            ,(local-file "files/enchant/enchant.ordering")))))
      ;; Ssh
      (list
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
                           (forward-agent? #t)))))))
      ;; Authinfo
      (list
       (simple-service 'krisb-symlink-authinfo
           home-activation-service-type
         #~(begin
             (use-modules (guix build utils))
             (let* ((source (string-append "/run/user/" (number->string (getuid)) "/secrets/.authinfo"))
                    (target (string-append (getenv "HOME") "/.authinfo")))
               (format #t "Directly symlinking .authinfo (~a) to ~a~%" source target)
               (when (false-if-exception (lstat target))
                 (delete-file target))
               (symlink source target)))))
      ;; SOPS
      (list
       (service home-sops-secrets-service-type
         (home-sops-service-configuration
           (config (local-file "files/sops/sops.yaml" "sops.yaml"))
           (secrets (list (sops-secret
                            (key '(".authinfo"))
                            (file
                             (local-file "files/sops/mute.yaml"))
                            ;; Make file unwritable, and only my user
                            ;; can read the file
                            (permissions #o400))
                          (sops-secret
                            (key '("gmi-credentials" "personal"))
                            (file
                             (local-file "files/sops/mute.yaml"))
                            (permissions #o400))
                          (sops-secret
                            (key '("gmi-credentials" "uni"))
                            (file
                             (local-file "files/sops/mute.yaml"))
                            (permissions #o400))
                          (sops-secret
                            (key '("restic-backup-password"))
                            (file
                             (local-file "files/sops/sublation.yaml"))
                            (permissions #o400)))))))
      ;; GPG
      (list
       (service home-gpg-agent-service-type
         (home-gpg-agent-configuration
           (pinentry-program
            (file-append pinentry "/bin/pinentry")))))
      ;; Jujutsu
      (list
       (simple-service 'krisb-symlink-jj-config-files-service-type
           home-xdg-configuration-files-service-type
         `(("jj/config.toml"
            ,(local-file "files/jujutsu/config.toml")))))
      ;; Git
      (list
       (simple-service 'krisb-symlink-git-config-files-service-type
           home-xdg-configuration-files-service-type
         `(("git/config"
            ,(local-file "files/git/config")))))
      ;; Atuin
      (list
       (simple-service 'krisb-symlink-atuin-config-files-service-type
           home-xdg-configuration-files-service-type
         `(("atuin/config.toml"
            ,(local-file "files/atuin/config.toml")))))
      ;; REVIEW 2025-07-02: Don't remember if needed on Guix system or
      ;; just a foreign distro.
      ;; Certificates
      (list
       (simple-service 'krisb-ssl-certs ; Requires nss-certs package
           home-environment-variables-service-type
         ;; NOTE: We install nss-certs via guix home, so SSL_CERT_DIR
         ;; is relative to ~/.guix-home/.  If nss-certs is installed
         ;; via e.g. guix install, then it would be relative to
         ;; ~/.guix-profile/.
         '(("SSL_CERT_DIR" . "$HOME/.guix-home/profile/etc/ssl/certs")
           ("SSL_CERT_FILE" . "$SSL_CERT_DIR/ca-certificates.crt")
           ("GIT_SSL_CAINFO" . "$SSL_CERT_FILE")
           ("CURL_CA_BUNDLE" . "$SSL_CERT_FILE"))))
      ;; Fish
      (list
       (service home-fish-service-type
         (home-fish-configuration
           ;; These are appended to ~/.config/fish/config.fish
           (config (list (local-file "files/fish/keychain.fish")
                         (plain-file "fish_greeting.fish" "set -g fish_greeting")
                         (plain-file "atuin_init.fish" "atuin init fish --disable-up-arrow | source")
                         (plain-file "zoxide_init.fish" "zoxide init fish | source")))
           (aliases `(("cat" . ,(string-join '("bat" "--theme=ansi"
                                               "--style=plain,header-filesize,grid,snip --paging auto"
                                               "--italic-text=always --nonprintable-notation=caret")))))
           (abbreviations '(("cd" . "z")))))
       (simple-service 'krisb-fisher
           ;; Install fisher if it isn't already installed, then
           ;; symlink fish_plugins, then update plugins.  We do this
           ;; altogether to ensure the operations are done in
           ;; sequence.
           home-activation-service-type
         #~(begin
             (use-modules (guix build utils))
             (let* ((source (canonicalize-path "config/files/fish/fish_plugins"))
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
             (system "fish -c \"fisher update\""))))
      ;; Bash
      (list
       (service home-bash-service-type
         (home-bash-configuration
           (aliases '(("grep" . "grep --color=auto")
                      ("la" . "ls -la")
                      ("ll" . "ls -l")
                      ("ls" . "ls -p --color=auto")))
           (bashrc (list (plain-file "atuin_init.bash" "eval '$(atuin init bash --disable-up-arrow)'")
                         (plain-file "zoxide_init.bash" "eval '$(zoxide init bash)'")))
           (bash-profile (list (local-file "files/bash/keychain.bash" "keychain.bash"))))))
      ;; Basic environment for all shells
      (list
       (simple-service 'basic-environment-service-type
           home-environment-variables-service-type
         '(("PAGER" . "less -RKF")
           ;; Use bat as a pager for man.  Taken from
           ;; https://github.com/sharkdp/bat?tab=readme-ov-file#man
           ("MANPAGER" . "sh -c 'sed -u -e \"s/\\x1B\\[[0-9;]*m//g; s/.\\x08//g\" | bat -p -lman'"))))
      ;; WSL2-specific
      (list
       (simple-service 'krisb-wslg-display-service-type
           home-environment-variables-service-type
         '(("DISPLAY" . ":0"))))
      ;; Guix on a foreign distro
      (list
       (simple-service 'krisb-foreign-distro
           home-environment-variables-service-type
         '(("GUIX_PROFILE" . "$HOME/.guix-profile"))))
      ;; Base services
      %base-home-services))))

krisb-home-environment
