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
  #:use-module (krisb services netbird)
  #:use-module (gnu services pm)
  #:use-module ((krisb config machines Maur hardware) #:prefix hardware:)
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

     (packages (cons* niri
                      %base-packages))
     (services
      (append (list (service power-profiles-daemon-service-type)
                    (service netbird-service-type
                      (netbird-configuration
                       (respawn? #f)))
                    (service gnome-desktop-service-type)
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

     (file-systems (cons* hardware:btrfs-subvolume-home
                          hardware:btrfs-subvolume-root
                          hardware:btrfs-subvolume-swap
                          hardware:partition-efi
                          %base-file-systems))
     ;; Activate swap devices (swapon); swapfiles/partitions must be
     ;; set up manually beforehand
     (swap-devices
      (list hardware:swap-space-hibernation
            hardware:swap-space-everyday))

     (kernel-arguments
      (append hardware:swap-kernel-parameters

              ;; Enable zswap.  For a comparison of zswap to zram, and
              ;; a justification of why zswap is preferrable to zram
              ;; and why zram shouldn't be used in conjunction with
              ;; zswap, see
              ;; https://linuxblog.io/zswap-better-than-zram/
              ;;
              ;; Run the command below to see the current value of all
              ;; zswap-related options:
              ;;   grep -r . /sys/module/zswap/parameters/
              ;;
              ;; To see the current stats of zswap, run:
              ;;   grep -r . /sys/kernel/debug/zswap/
              (list "zswap.enabled=1"
                    "zswap.max_pool_percent=25"
                    "zswap.shrinker_enabled=1")

              %default-kernel-arguments)))))
