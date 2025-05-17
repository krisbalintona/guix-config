(define-module (conf wsl)
  #:use-module (gnu)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu packages version-control)
  #:use-module (guix channels)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1))

(use-modules (gnu)
	     (gnu packages shells)
	     (gnu services)
             (gnu services shepherd)
             (guix gexp)
	     (gnu packages base))	; For coreutils

;; Workaround for WSL2.  See
;; https://github.com/microsoft/WSL/issues/11261
(define %xdg-runtime-dir "/run/user/1000")

(define symlink-wayland-service
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

(define symlink-wayland-service-type
  (service-type
   (name 'symlink-wayland)
   (description "Workaround for symlinking /mnt/wslg/ wayland sockets to XDG_RUNTIME_DIR.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (const (list symlink-wayland-service)))))
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

    (services (list (service symlink-wayland-service-type)
		    (service guix-service-type)
                    (service special-files-service-type
                             `(("/usr/bin/env" ,(file-append coreutils "/bin/env"))))))))

wsl-operating-system
