;; This "home-environment" file can be passed to 'guix home reconfigure' to
;; reproduce the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to capture
;; the channels being used, as returned by "guix describe".  See the
;; "Replicating Guix" section in the manual.

(define-module (conf home-configuration)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages crypto)
  #:use-module (gnu services)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (krisb packages jujutsu)
  #:use-module (krisb packages fonts)
  #:use-module (krisb packages atuin)
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
               ;; Emacs
               ;; 2025-05-21: Custom Emacs build.  I use pgtk for
               ;; support of the alpha-background frame parameter.
               ;; emacs-master-pgtk also suffices for this purpose
               "emacs-master-custom"
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
               )))

   (services
    (append (list
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
             ;; Config files
             (simple-service 'symlink-config-files-service-type
                             home-xdg-configuration-files-service-type
                             `(("git/config" ,(local-file "files/git/config"))
                               ("jj/config.toml"
                                ,(local-file "files/jujutsu/config.toml"))
                               ("atuin/config.toml"
                                ,(local-file "files/atuin/config.toml"))))
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
                                     ;; TODO 2025-05-21: Is there a
                                     ;; way to not have to create
                                     ;; this small, specialized file?
                                     (local-file "files/atuin/atuin_init.fish")))))
             (service home-bash-service-type
                      (home-bash-configuration
                       (aliases '(("grep" . "grep --color=auto")
                                  ("la" . "ls -la")
                                  ("ll" . "ls -l")
                                  ("ls" . "ls -p --color=auto")))
                       (bashrc (list (local-file "files/bash/.bashrc" "bashrc")
                                     ;; TODO 2025-05-21: Is there a
                                     ;; way to not have to create
                                     ;; this small, specialized file?
                                     (local-file "files/atuin/atuin_init.bash")))
                       (bash-profile (list (local-file
                                            "files/bash/.bash_profile"
                                            "bash_profile"))))))
            %base-home-services))))

krisb-home-environment
