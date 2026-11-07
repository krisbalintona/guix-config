(define-module (krisb config machines Maur system)
  #:use-module (gnu)
  #:use-module (gnu packages window-management)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages shells)
  #:use-module (gnu services nix)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (nonguix transformations)
  #:export (Maur-operating-system))

(define Maur-operating-system
  ((compose (nonguix-transformation-guix)
            (nonguix-transformation-linux))
   (operating-system
     (locale "en_US.utf8")
     (timezone "America/Chicago")
     (keyboard-layout (keyboard-layout "us" "altgr-intl"))
     (host-name "Maur")

     (users (cons* (user-account
                     (name "krisbalintona")
                     (comment "Kristoffer Balintona")
                     (group "users")
                     (home-directory "/home/krisbalintona")
                     (supplementary-groups '("wheel" "netdev" "audio" "video"))
		     (shell (file-append fish "/bin/fish")))
                   %base-user-accounts))

     (services
      (append (list (service gnome-desktop-service-type)
		    (service nftables-service-type)
		    (service nix-service-type)
                    (service openssh-service-type)
                    (service cups-service-type)
                    (set-xorg-configuration
                     (xorg-configuration (keyboard-layout keyboard-layout))))
              %desktop-services))
     (bootloader (bootloader-configuration
                   (bootloader grub-efi-bootloader)
                   (targets (list "/boot/efi"))
                   (keyboard-layout keyboard-layout)))

     (file-systems (cons* (file-system
                            (mount-point "/")
                            (device (uuid
                                     "3a9b954f-6eca-4272-baa2-9d7554d118bc"
                                     'btrfs))
                            (type "btrfs")
                            (options "subvol=@,compress=zstd"))
                          (file-system
                            (mount-point "/home")
                            (device (uuid
                                     "3a9b954f-6eca-4272-baa2-9d7554d118bc"
                                     'btrfs))
                            (type "btrfs")
                            (options "subvol=@home,compress=zstd"))
                          (file-system
                            (mount-point "/boot/efi")
                            (device (uuid "2037-86C8"
                                          'fat32))
                            (type "vfat")) %base-file-systems)))))
