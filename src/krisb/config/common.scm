(define-module (krisb config common)
  #:use-module (guix gexp)     ; Provides local-file, plain-file, etc.
  #:use-module (guix build utils)
  #:use-module (gnu packages)
  #:use-module (gnu services)           ; Provides simple-service
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (krisb services utils)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu home services gnupg)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (sops home services sops)
  #:use-module (gnu services backup)
  #:use-module (gnu home services backup)
  #:use-module (gnu home services syncthing)
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

(define-public sops-mute-secrets-path
  (config-files-path "sops/mute.yaml"))

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
;; Devices
(define-public syncthing-sublation-arch-device
  (syncthing-device
    (id "IEIGZYX-QJGTCMC-LTWQ6S4-2RB77GM-WOXUBMM-J26I7LO-KIROD5R-XP7LXAT")
    (name "Sublation (laptop server, Guix)")))

(define-public syncthing-guix-arch-device
  (syncthing-device
    (id "UIVVWP3-XZ2GV2F-7N3RMXC-FRIKYXP-Y2MAXNA-JMGD45T-AZYZZJC-FBFSQQ5")
    (name "Mute (G14 2024, Guix in Arch))")))

(define-public syncthing-wsl-arch-device
  (syncthing-device
    (id "OQHSZRW-L2TT7IC-7USSLNU-ST7JYML-J7J6CU3-42P7NCA-WHE7BEL-SASRXA3")
    (name "G14 2024 Arch WSL")))

(define-public syncthing-one-plus-7-pro-device
  (syncthing-device
    (id "OVGYOBF-JPFQJKE-6CKRY7J-JULRCWK-WSGSA6Y-SQZYLLE-B2OLSDJ-6DRSTQZ")
    (name "OnePlus 7 Pro")))

;; Folder IDs
(define-public syncthing-notes-folder-id "qtuzy-ufufb")

(define-public syncthing-agenda-folder-id "k4vqh-rny7b")

(define-public syncthing-biblio-folder-id "kjtm2-zyajn")



(define-public common-home-packages
  (specifications->packages
   (list
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
    )))

(define-public common-system-services
  (list
   (service openssh-service-type)
   ))

(define-public common-home-services
  (list
   (service home-restic-backup-service-type) ; Need this service in order to extend it in other services
   ))
