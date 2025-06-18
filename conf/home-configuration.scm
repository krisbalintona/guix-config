;; This "home-environment" file can be passed to 'guix home reconfigure' to
;; reproduce the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to capture
;; the channels being used, as returned by "guix describe".  See the
;; "Replicating Guix" section in the manual.

(define-module (conf home-configuration)
  #:use-module (guix gexp)
  #:use-module (guix utils)
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
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services dict)
  #:use-module (gnu home services gnupg)
  #:use-module (sops secrets)
  #:use-module (sops home services sops)
  #:use-module (krisb packages jujutsu)
  #:use-module (krisb packages fonts)
  #:use-module (krisb packages atuin)
  #:use-module (krisb packages lieer)
  #:use-module (krisb packages emacs))

(define krisb-home-environment
  (home-environment
   ;; Below is the list of packages that will show up in your Home profile,
   ;; under ~/.guix-home/profile.
   (packages (specifications->packages
              (list
               ;; Basic stuff
               "coreutils"
               "findutils"
               "grep"
               "sed"
               "gawk"
               "less"
               "which"
               "gzip"
               "bzip2"
               "xz"
               "tar"
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
               "neovim"
               "fontconfig"
               "fzf"
               "ripgrep"
               "font-iosevka"
               "xdg-utils"
               "xdg-user-dirs"
               "make"
               "atuin-bin" ; Don't forget to log in and sync atuin on first install
               "wl-clipboard"
               "python-lieer"
               "l2md"
               "zotero"
               "python"
               "gnupg" "pinentry" "sops"
               "mpv"
               ;; Emacs
               ;; 2025-05-21: Custom Emacs build.  I use pgtk for
               ;; support of the alpha-background frame parameter.
               ;; emacs-master-pgtk also suffices for this purpose
               "emacs-master-igc-custom"
               "emacs-guix"
               "emacs-arei" "guile-next" "guile-ares-rs"
               "jujutsu-bin"
               "patch"                  ; Needed for vc-jj
               "emacs-pdf-tools"
               "enchant" "emacs-jinx"
               "aspell" "aspell-dict-en"
               "hunspell" "hunspell-dict-en" "hunspell-dict-en-us"
               "nuspell"
               "hugo"
               "texlive-latexmk"
               "emacs-lsp-booster"
               "emacs-nerd-icons"
               "notmuch"
               "xmodmap"
               "font-google-noto-emoji" ; For emojis
               "font-iosevka"
               "font-iosevka-aile-nerd-font"
               "font-iosevka-term-ss04-nerd-font"
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
               "vale" "python-proselint")))

   (services
    (append (list
             ;; SOPS
             (service home-sops-secrets-service-type
                      (home-sops-service-configuration
                       (config
                        (local-file "files/sops/.sops.yaml"
                                    ;; Paths in the store cannot start
                                    ;; with dots
                                    "sops.yaml"))
                       (secrets
                        (list
                         (sops-secret
                          (key '(".authinfo"))
                          (file
                           (local-file "files/sops/secrets.yaml"))
                          ;; Make file unwritable, and only my user
                          ;; can read the file
                          (permissions #o400))))))
             (simple-service 'symlink-sops-files
                             home-files-service-type
                             `((".authinfo"
                                ,(local-file (string-append "/run/user/" (number->string (getuid)) "/secrets/.authinfo")
                                             ;; Paths in the store
                                             ;; cannot start with dots
                                             "authinfo"))))
             ;; GPG
             (service home-gpg-agent-service-type
                      (home-gpg-agent-configuration
                       (pinentry-program
                        (file-append pinentry "/bin/pinentry"))))
             ;; Maintain notmuch database
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
                                              ;; Run every 15 minutes.
                                              ;; Cannot run too
                                              ;; frequently, otherwise
                                              ;; risk an 504 timeout
                                              ;; error when trying to
                                              ;; fetch emails.
                                              "*/15 * * * *"
                                              #~("sh" "-c"
                                                 #$(string-join '("echo 'Starting l2md fetch...'" "&&"
                                                                  "l2md" "--verbose" "&&"
                                                                  "echo 'Done fetching!'" "&&"
                                                                  "echo 'Starting notmuch new...'" "&&"
                                                                  "notmuch" "new" "&&"
                                                                  "echo 'All done!'"))))))
             ;; Syncing emails with lieer
             (simple-service 'gmi-sync
                             home-shepherd-service-type
                             (let ((personal-email-dir "Documents/emails/personal/")
                                   (uni-email-dir "Documents/emails/personal/")
                                   ;; For gmi sync, use --verbose to
                                   ;; have the most useful log
                                   ;; messages, and use --resume such
                                   ;; that in the case of a sudden
                                   ;; power off, the .lock file will
                                   ;; not cause a lock up.  Of course,
                                   ;; this may lead to the local
                                   ;; changes to be ignored until the
                                   ;; next sync, but that is
                                   ;; preferable to my mail not being
                                   ;; synced unknowingly.
                                   (gmi-sync "gmi sync --verbose")
                                   ;; This restores the gmi internal
                                   ;; state if gmi sync fails; this is
                                   ;; like a “last resort.”
                                   (gmi-rescue "cp .state.gmailieer.json{.bak,}")
                                   ;; Show as much as possible in the
                                   ;; logs.
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
             ;; FIXME 2025-05-24: Currently does not work, at least in
             ;; WSLg where I am writing this.  Although the non-inetd
             ;; mode version works (tested while running manually in
             ;; the CLI), inetd mode gives troubles.  For now I leave
             ;; this in, since my guesss is that it should be
             ;; sufficient in non-WSLg systems.
             ;; Dictd (dictionary server implementation)
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
             ;; Syncthing
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
                          (user "krisbalintona") ; My user
                          (config-file
                           (syncthing-config-file
                            ;; We use a non-standard port because we are on WSL
                            ;; with other distros and we want them using
                            ;; different ports
                            (gui-address "127.0.0.1:8386")
                            (folders (list agenda-folder notes-folder))))))))
             ;; Ssh
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
                                       (identity-file "~/.ssh/id_ed25519"))))))
             ;; Vale
             (simple-service 'symlink-vale-config-file-service-type
                             home-xdg-configuration-files-service-type
                             `(("vale/.vale.ini"
                                ,(local-file "files/vale/vale.ini"))))
             (simple-service 'symlink-vale-styles-service-type
                             home-files-service-type
                             `((".local/share/vale/styles/krisb-custom"
                                ,(local-file "files/vale/krisb-custom" #:recursive? #t))))
             ;; Config files
             (simple-service 'symlink-config-files-service-type
                             home-xdg-configuration-files-service-type
                             `(("git/config"
                                ,(local-file "files/git/config"))
                               ("jj/config.toml"
                                ,(local-file "files/jujutsu/config.toml"))
                               ("atuin/config.toml"
                                ,(local-file "files/atuin/config.toml"))
                               ("notmuch/default"
                                ,(local-file "files/notmuch" #:recursive? #t))
                               ("l2md/config"
                                ,(local-file "files/l2md/config"))))
             ;; WSL2-specific
             (simple-service 'krisb-wslg-display-service-type
                             home-environment-variables-service-type
                             '(("DISPLAY" . ":0")))
             (simple-service 'krisb-ssl-certs
                             home-environment-variables-service-type
                             '(("SSL_CERT_DIR" . "$HOME/.guix-profile/etc/ssl/certs")
                               ("SSL_CERT_FILE" . "$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt")
                               ("GIT_SSL_CAINFO" . "$SSL_CERT_FILE")
                               ("CURL_CA_BUNDLE" . "$SSL_CERT_FILE")))
             ;; Shells
             (service home-fish-service-type
                      (home-fish-configuration
                       ;; These are appended to ~/.config/fish/config.fish
                       (config (list (local-file "files/fish/keychain.fish")
                                     (local-file "files/atuin/atuin_init.fish")))))
             (service home-bash-service-type
                      (home-bash-configuration
                       (aliases '(("grep" . "grep --color=auto")
                                  ("la" . "ls -la")
                                  ("ll" . "ls -l")
                                  ("ls" . "ls -p --color=auto")))
                       (bashrc (list (local-file "files/atuin/atuin_init.bash")))
                       (bash-profile (list (local-file "files/bash/keychain.bash" "keychain.bash"))))))
            %base-home-services))))

krisb-home-environment
