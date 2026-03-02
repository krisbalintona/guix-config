(define-module (conf wsl)
  #:use-module (gnu)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu packages version-control)
  #:use-module (guix channels)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages shells)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs))

;; Workarounds for WSL2
(define %xdg-runtime-dir "/run/user/1000")

;; See https://github.com/microsoft/WSL/issues/11261
(define krisb-symlink-wayland-service
  (shepherd-service
   (provision '(symlink-wayland-socket))
   (documentation "Symlink Wayland socket to XDG_RUNTIME_DIR for WSL2.")
   (one-shot? #t)
   (start #~(lambda _
              (use-modules (ice-9 popen)
                           (ice-9 format))
              (format #t "Making ~a if necessary..." #$%xdg-runtime-dir)
              (mkdir-p #$%xdg-runtime-dir)
              (format #t "Starting symlinking...\n")
              (system* #$(file-append coreutils "/bin/ln") "-sf"
                       "/mnt/wslg/runtime-dir/wayland-0"
                       (string-append #$%xdg-runtime-dir "/wayland-0"))
              (system* #$(file-append coreutils "/bin/ln") "-sf"
                       "/mnt/wslg/runtime-dir/wayland-0.lock"
                       (string-append #$%xdg-runtime-dir "/wayland-0.lock"))
              (format #t "Symlinked successfully!\n")
              #t))))

;; See https://github.com/microsoft/WSL/issues/10846
(define krisb-xdg-runtime-dir-ownership-service
  (shepherd-service
   (provision '(fix-xdg-runtime-dir-ownership))
   (requirement '(symlink-wayland-socket))
   (documentation "Ensure correct ownership and permissions of XDG_RUNTIME_DIR for WSL2.")
   (one-shot? #t)
   (start #~(lambda _
              (use-modules (ice-9 format))
              (let ((dir #$%xdg-runtime-dir))
                (format #t "Ensuring directory exists: ~a\n" dir)
                (mkdir-p dir)
                (format #t "Changing ownership to UID 1000, GID 1000...\n")
                ;; TODO: Don’t hardcode my personal user?
                (system* "sudo" "chown" "-R" "krisbalintona:users" dir)
                (format #t "Setting permissions to 700...\n")
                (system* "sudo" "chmod" "-R" "700" dir)
                (format #t "All done!\n")
                #t)))))

(define krisb-wsl-correct-xdg-runtime-dir-service-type
  (service-type
   (name 'wsl-corrections)
   (description "Workaround for symlinking /mnt/wslg/ wayland sockets to XDG_RUNTIME_DIR.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (const (list krisb-symlink-wayland-service
                                          krisb-xdg-runtime-dir-ownership-service)))))
   (default-value #f)))

(define-public wsl-operating-system
  (operating-system
   (host-name "Guix-WSL")
   (timezone "America/Chicago")
   (locale "en_US.utf8")

   ;; User accounts
   (users (cons (user-account
                 (name "krisbalintona")
                 (group "users")
                 (supplementary-groups '("wheel"))
                 (shell (file-append fish "/bin/fish")))
                %base-user-accounts))

   (kernel hello)
   (initrd (lambda* (. rest) (plain-file "dummyinitrd" "dummyinitrd")))
   (initrd-modules '())
   (firmware '())

   (bootloader
    (bootloader-configuration
     (bootloader
      (bootloader
       (name 'dummybootloader)
       (package hello)
       (configuration-file "/dev/null")
       (configuration-file-generator (lambda* (. rest) (computed-file "dummybootloader" #~(mkdir #$output))))
       (installer #~(const #t))))))

   (file-systems (list (file-system
                        (device "/dev/sdb")
                        (mount-point "/")
                        (type "ext4")
                        (mount? #t))))

   (packages
    (append (list)
            %base-packages))

   (services (list (service krisb-wsl-correct-xdg-runtime-dir-service-type)
                   (service guix-service-type)
                   (service special-files-service-type
                            `(("/usr/bin/env" ,(file-append coreutils "/bin/env"))))))))

wsl-operating-system
