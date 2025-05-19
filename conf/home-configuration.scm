;; This "home-environment" file can be passed to 'guix home reconfigure' to
;; reproduce the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to capture
;; the channels being used, as returned by "guix describe".  See the
;; "Replicating Guix" section in the manual.

(define-module (conf home-configuration)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages monitoring)
  :use-module (gnu packages crypto)
  #:use-module (gnu home services ssh)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (krisb packages jujutsu)
  #:use-module (krisb packages fonts))

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
	       "xdg-user-dirs"
	       "make"
               ;; Emacs
               "emacs-master"
               "emacs-guix"
               "emacs-arei" "guile-next" "guile-ares-rs"
               "jujutsu-bin"
               "patch"                  ; Needed for vc-jj
               "emacs-pdf-tools"
               "enchant" "emacs-jinx" "gcc-toolchain"
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
	       "texinfo"		; To make info files
	       "plocate" 		; For consult
	       )))

   (services
    (append (list
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
                                ,(local-file "files/jujutsu/config.toml"))))
             ;; WSL2-specific
             (simple-service 'krisb-wslg-display-service-type
                             home-environment-variables-service-type
                             '(("DISPLAY" . ":0")))
             (simple-service 'krisb-ssl-certs
                             home-environment-variables-service-type
                             '(("SSL_CERT_DIR" . "$HOME/.guix-profile/etc/ssl/certs")
                               ("SSL_CERT_FILE" . "$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt")
                               ("GIT_SSL_CAINFO" . "$SSL_CERT_FILE")
                               ("CURL_CA_BUNDLE" . "$SSL_CERT_FILE")
                               ))
             ;; Shells
             (service home-fish-service-type
                      (home-fish-configuration
                       ;; These are appended to ~/.config/fish/config.fish
                       (config (list (local-file
                                      "files/fish/keychain.fish"
                                      "keychain_fish")))))
             (service home-bash-service-type
                      (home-bash-configuration
                       (aliases '(("grep" . "grep --color=auto")
                                  ("la" . "ls -la")
                                  ("ll" . "ls -l")
                                  ("ls" . "ls -p --color=auto")))
                       (bashrc (list (local-file "files/bash/.bashrc" "bashrc")))
                       (bash-profile (list (local-file
                                            "files/bash/.bash_profile"
                                            "bash_profile"))))))
            %base-home-services))))

krisb-home-environment
