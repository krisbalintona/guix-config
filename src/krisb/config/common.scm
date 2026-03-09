(define-module (krisb config common)
  #:use-module (guix gexp)     ; Provides local-file, plain-file, etc.
  #:use-module (guix build utils)
  #:use-module (gnu packages)
  #:use-module (gnu services)           ; Provides simple-service
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages shells)
  #:use-module (gnu home services shells)
  #:use-module (krisb services shells)
  #:use-module (gnu packages compression)
  #:use-module (abbe packages rust)
  #:use-module (krisb services shells)
  #:use-module (abbe packages rust)
  #:use-module (gnu packages terminals)                ; fzf
  #:use-module (gnu packages rust-apps)                ; fd
  #:use-module (krisb services shells)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu home services gnupg)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (sops home services sops)
  #:use-module (gnu services backup)
  #:use-module (gnu home services backup)
  #:use-module (krisb packages lieer)
  )

(define-public %config-files-dir
  ;; The cwd should be the repository root
  (string-append (getcwd) "/files"))

(define-public (config-files-path path)
  (string-append %config-files-dir "/" path))

(define-public sops-mute-secrets-file
  (local-file (config-files-path "sops/mute.yaml")))

;; Make sure the SSH key associated with "sublation-backup" (in
;; ~/.ssh/config) is an authorized key on the remote (i.e., present in
;; ~/.ssh/authorized_keys).  Otherwise SSH attempts will still prompt
;; for a password (and therefore error), even with passwordless SSH
;; keys.
(define-public sops-mute-repository-path "sftp:sublation-backup:/mnt/backup-hdd")

(define-public sops-sublation-secrets-path
  (config-files-path "sops/sublation.yaml"))

;; TODO 2026-03-07: Make path identical to mount path of drive
;; (define-public sops-sublation-repository-path "/mnt/backup-hdd")
(define-public sops-sublation-repository-path sops-mute-repository-path)
;; Get secrets as strings.  Taken from
;; https://github.com/fishinthecalculator/sops-guix/issues/2
(use-modules ((ice-9 popen) #:select (open-input-pipe close-pipe))
             ((rnrs io ports) #:select (get-string-all))
             ((sops secrets) #:select (sanitize-sops-key)))

(define* (get-sops-secret key #:key file (number? #f))
  (let* ((cmd (format #f "sops --decrypt --extract '~a' '~a'"
                      (sops-list-key->sops-string-key key)
                      file))
         (port (open-input-pipe cmd))
         (secret (get-string-all port)))
    (close-pipe port)
    (if number?
        (string->number secret)
        secret)))
(export get-sops-secret)

;; Helper for getting file path of secret
(define-public (get-sops-secret-path filename)
  (string-append "/run/user/" (number->string (getuid))
                 "/secrets/" filename))
(define-public restic-password-secret
  (sops-secret
    (key '("restic-backup-password"))
    (file (local-file sops-sublation-secrets-path))
    (permissions #o400)))

(define* (restic-job/defaults
          #:key
          (restic (@ (abbe packages golang) restic)) ; More up-to-date Restic
          name
          repository
          (password-file
           (sops-secret->secret-file restic-password-secret))
          files
          schedule
          (wait-for-termination? #t)
          (extra-flags '("--retry-lock" "15m"))
          (verbose? #t))
  (restic-backup-job
    (restic restic)
    (name name)
    (files files)
    (schedule schedule)
    (repository repository)
    (password-file password-file)
    (wait-for-termination? wait-for-termination?)
    (extra-flags extra-flags)
    (verbose? verbose?)))
(export restic-job/defaults)

(define-public common-home-packages
  (specifications->packages
   (list
    "bash-completion"
    "grc"                          ; For oh-my-fish/plugin-grc fish plugin
    "coreutils"
    "findutils"
    "diffutils"
    "file"
    "grep"
    "sed"
    "less"
    "which"
    "btop"
    "glances"
    "nmon"
    "atop"
    "git"
    "make"
    "cmake"
    "python"
    "direnv"
    "tree"
    "ripgrep"
    "fd"
    "jq"
    "rsync"
    "parted"
    "unzip"
    "zip"
    "gzip"
    "bzip2"
    "xz"
    "tar"
    "bat"
    "procs"
    "jujutsu"
    "zellij"
    "ffmpeg"                          ; Diagnose video info (e.g., codecs)
    "mediainfo"
    "guile-readline"
    "guile-colorized"
    "gnupg"
    "age"
    "keychain"
    "inetutils"
    "net-tools"
    "curl"
    "wget"
    "nmap"                                  ; Port scanning
    "masscan"                               ; Port scanning
    "restic"
    "xdg-utils"
    "xdg-user-dirs"
    "fontconfig"
    )))

(define-public common-home-services
  (list
   (simple-service 'home-fish-bat
       home-fish-service-type
     (home-fish-extension
       (aliases
        `(("cat" . ,(string-join '("bat" "--theme=ansi"
                                   "--style=plain,header-filesize,grid,snip --paging auto"
                                   "--italic-text=always --nonprintable-notation=caret")))))))
   (simple-service 'pager-environment-variables
       home-environment-variables-service-type
     ;; Use bat as a pager for man.  Taken from
     ;; https://github.com/sharkdp/bat?tab=readme-ov-file#man
     '(("MANPAGER" . "sh -c 'sed -u -e \"s/\\x1B\\[[0-9;]*m//g; s/.\\x08//g\" | bat -p -lman'")))
   (service home-zoxide-service-type
     (home-zoxide-configuration
       (zoxide (@ (abbe packages rust) zoxide))))
   (simple-service 'home-fish-zoxide
       home-fish-service-type
     (home-fish-extension
       (abbreviations '(("cd" . "z")))))
   (simple-service 'home-fish-procs
       home-fish-service-type
     (home-fish-extension
       (abbreviations `(("ps" . "procs")))))
   (simple-service 'krisb-symlink-git-config-files-service-type
       home-xdg-configuration-files-service-type
     `(("git/config"
        ,(local-file (config-files-path "git/config")))))
   (simple-service 'jj-config-files-service-type
         home-xdg-configuration-files-service-type
       `(("jj/config.toml"
          ,(local-file (config-files-path "jujutsu/config.toml")))))
   (simple-service 'fish-vcs-jj
         home-xdg-configuration-files-service-type
       `(("fish/functions/fish_jj_prompt.fish"
          ,(local-file (config-files-path "jujutsu/fish_jj_prompt.fish")))
         ("fish/functions/fish_vcs_prompt.fish"
          ,(local-file (config-files-path "jujutsu/fish_vcs_prompt.fish")))))
   (simple-service 'fish-fzf-packages
       home-profile-service-type
     (list fd fzf))                        ; Function dependencies
   (simple-service 'fish-fzf-function
         home-xdg-configuration-files-service-type
       `(("fish/functions/fzf_complete.fish"
          ,(local-file (config-files-path "fish/fzf_complete.fish")))))
   (simple-service 'fish-fzf-config
       home-fish-service-type
     (home-fish-extension
       (config
        (list (plain-file "fzf_custom.fish" "bind \\t fzf_complete")))))
   (service home-atuin-service-type
     (home-atuin-configuration
       (atuin-fish-flags '("--disable-up-arrow"))
       (atuin-bash-flags '("--disable-up-arrow"))))
   (simple-service 'krisb-symlink-atuin-config-files-service-type
       home-xdg-configuration-files-service-type
     `(("atuin/config.toml"
        ,(local-file (config-files-path "atuin/config.toml")))))
   (service home-dotfiles-service-type
     (home-dotfiles-configuration
       (source-directory %config-files-dir)
       (layout 'plain)
       (directories (list "scripts"))))
   (simple-service 'files-dotguile
       home-files-service-type
     `((".guile" ,%default-dotguile)))
   (simple-service 'home-bash-keychain
       home-bash-service-type
     (home-bash-extension
       (bash-profile (list (local-file (config-files-path "bash/keychain.bash") "keychain.bash")))))
   (simple-service 'home-fish-keychain
       home-fish-service-type
     (home-fish-extension
       (config
        (list (local-file (config-files-path "fish/keychain.fish"))))))
   (service home-restic-backup-service-type) ; Need this service in order to extend it in other services
   ))
