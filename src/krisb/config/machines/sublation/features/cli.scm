(define-module (krisb config machines sublation features cli)
  #:use-module (guix gexp)

  #:use-module (gnu packages base)
  #:use-module ((gnu packages file) #:select (file))
  #:use-module ((gnu packages less) #:select (less))
  #:use-module ((gnu packages disk) #:select (parted))
  #:use-module ((gnu packages cmake) #:select (cmake))
  #:use-module (gnu packages video)
  #:use-module ((gnu packages admin) #:select (tree btop nmon atop))
  #:use-module (gnu packages compression)
  #:use-module ((gnu packages terminals) #:select (fzf))
  #:use-module ((gnu packages rust-apps) #:select (fd ripgrep procs))
  #:use-module ((gnu packages web) #:select (jq))
  #:use-module ((gnu packages rsync) #:select (rsync))
  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages monitoring) #:select (glances))
  #:use-module ((gnu packages linux) #:select (brightnessctl))

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)

  #:use-module (krisb services utils)
  #:use-module (krisb services shells)
  #:use-module (krisb config common)
  #:export (feature-cli-essentials
            feature-software-development
            feature-file-archivers
            feature-system-monitoring
            feature-zoxide
            feature-bat
            feature-procs
            feature-hardware-control
            feature-git
            feature-jujutsu
            feature-fzf
            feature-atuin
            feature-zellij
            feature-direnv))


;;;
;;; Essentials
;;;

(define (feature-cli-essentials)
  (list (simple-service 'cli-essentials
            home-profile-service-type
          (list coreutils
                findutils
                diffutils
                file
                grep
                sed
                less
                which
                parted))))


;;;
;;; General software development
;;;
;; File searching, visualization, file transfer, build systems, and
;; programming languages

(define (feature-software-development)
  (list (simple-service 'cli-essentials
            home-profile-service-type
          (list tree
                ripgrep
                fd
                jq
                rsync
                git
                gnu-make                ; make
                cmake
                python))))


;;;
;;; Compression and file archivers
;;;

(define (feature-file-archivers)
  (list (simple-service 'cli-essentials
            home-profile-service-type
          (list unzip
                zip
                gzip
                bzip2
                xz
                tar))))


;;;
;;; System monitoring
;;;

(define (feature-system-monitoring)
  (list (simple-service 'cli-essentials
            home-profile-service-type
          (list btop
                glances
                nmon
                atop))))


;;;
;;; Zoxide
;;;

(define (feature-zoxide)
  (list (simple-service 'zoxide
            home-zoxide-service-type
          (home-zoxide-configuration
            (zoxide (@ (abbe packages rust) zoxide))))
        (simple-service 'zoxide-fish
            home-fish-service-type
          (home-fish-extension
            (abbreviations '(("cd" . "z")))))))


;;;
;;; Bat
;;;
;; Configurable. Options include output with syntax highlighting, line
;; numbers, and automatic piping to 'less'.

(define (feature-bat)
  (list (simple-service 'bat
            home-profile-service-type
          (list (@ (abbe packages rust) bat)))
        (simple-service 'bat-fish
            home-fish-service-type
          (home-fish-extension
            (aliases
             `(("cat" . ,(string-join '("bat" "--theme=ansi"
                                        "--style=plain,header-filesize,grid,snip --paging auto"
                                        "--italic-text=always --nonprintable-notation=caret")))))))
        (simple-service 'bat-env-vars
            home-environment-variables-service-type
          ;; Use bat as a pager for man.  Taken from
          ;; https://github.com/sharkdp/bat?tab=readme-ov-file#man
          '(("MANPAGER" . "sh -c 'sed -u -e \"s/\\x1B\\[[0-9;]*m//g; s/.\\x08//g\" | bat -p -lman'")))))


;;;
;;; Procs
;;;
;; A fancier 'ps'

(define (feature-procs)
  (list (simple-service 'procs
            home-profile-service-type
          (list procs))
        (simple-service 'procs-fish
            home-fish-service-type
          (home-fish-extension
            (abbreviations `(("ps" . "procs")))))))


;;;
;;; Hardware control
;;;

(define (feature-hardware-control)
  (list (simple-service 'procs
            home-profile-service-type
          (list brightnessctl))))


;;;
;;; Git
;;;

(define (feature-git)
  (list (simple-service 'git-config-files
            home-xdg-configuration-files-service-type
          `(("git/config" ,(local-file (config-files-path "git/config")))))))


;;;
;;; Jujutsu
;;;
;; Distributed VCS (more ergonomic than Git!)

(define (feature-jujutsu)
  (list (simple-service 'jujutsu
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


;;;
;;; Fzf
;;;

(define (feature-fzf)
  (list (simple-service 'fzf
            home-profile-service-type
          (list fd fzf))        ; Dependency
        ;; Bespoke integration with Fish.  The initial inspiration for
        ;; this was https://github.com/gazorby/fifc
        (simple-service 'fzf-fish-custom-complete
            home-xdg-configuration-files-service-type
          `(("fish/functions/fzf_complete.fish"
             ,(local-file (config-files-path "fish/fzf_complete.fish")))))
        (simple-service 'fzf-fish-custom-complete-binding
            home-fish-service-type
          (home-fish-extension
            (config
             (list (plain-file "fzf_custom.fish" "bind \\t fzf_complete")))))))


;;;
;;; Atuin
;;;
;; Share shell history across shells and machines
;;
;; Note regarding setup: don't forget to register then log in to sync
;; history across machines; see https://docs.atuin.sh/cli/guide/sync/.
;;
;; Alternatives include:
;; - https://github.com/cantino/mcfly (see also
;;   https://github.com/bnprks/mcfly-fzf)
;; - https://github.com/ddworken/hishtory
;;
;; TODO 2026-01-04: Consider self-hosting a sync server. See
;; https://docs.atuin.sh/cli/self-hosting/server-setup/

(define (feature-atuin)
  (list (let ((flags '("--disable-up-arrow")))
          (simple-service 'atuin
              home-atuin-service-type
            (home-atuin-configuration
              (atuin-fish-flags flags)
              (atuin-bash-flags flags))))
        (simple-service 'atuin-config-files
            home-xdg-configuration-files-service-type
          `(("atuin/config.toml"
             ,(local-file (config-files-path "atuin/config.toml")))))))


;;;
;;; Zellij
;;;
;; A batteries-included tmux alternative.  What fish is to zsh but for
;; tmux.

(define (feature-zellij)
  (list (simple-service 'zellij
            home-profile-service-type
          (list (@ (abbe packages rust) zellij)))
        (simple-service 'zellij-config-files
            direct-symlink-service-type
          (list (direct-symlink-configuration
                 (link-path
                  #~(string-append (or (getenv "XDG_CONFIG_HOME")
                                       (string-append (getenv "HOME") "/.config"))
                                   "/zellij/config.kdl"))
                 (target-path
                  (config-files-path "zellij/config.kdl")))))))


;;;
;;; Direnv
;;;
;; Directory-local environments

(define (feature-direnv)
  (list (simple-service 'direnv
            home-profile-service-type
          (list (@ (abbe packages golang) direnv/update)))
        ;; Fish
        (simple-service 'direnv-fish
            home-fish-service-type
          (home-fish-extension
            (config
             (list (plain-file "direnv_setup.fish" "direnv hook fish | source")))))
        ;; Bash
        (simple-service 'direnv-bash
            home-bash-service-type
          (home-bash-extension
            (bashrc
             (list (plain-file "direnv_setup.bash" "eval \"$(direnv hook bash)\"")))))))


;;;
;;; Audio and video codecs tools
;;;

(define (feature-codecs)
  (list (simple-service 'codecs
            home-profile-service-type
          (list ffmpeg mediainfo))
        (simple-service 'codecs-config-files
            home-dotfiles-service-type
          (home-dotfiles-configuration
            (source-directory %config-files-dir)
            (layout 'plain)
            (directories (list "scripts"))))))
