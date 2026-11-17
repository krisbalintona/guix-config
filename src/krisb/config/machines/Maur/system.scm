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
  #:use-module (krisb config machines Maur hardware)
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
              (modify-services %desktop-services
                ;; I use the 'home-niri-service-type' in my Guix Home
                ;; configuration.  As explained in (guix) Niri window
                ;; manager, 'home-niri-service-type' does not work
                ;; with 'gdm-service-type'.
                ;;
                ;; There is work upstream that proposes a change in
                ;; the 'home-niri-service-type' to make them
                ;; compatible:
                ;; https://codeberg.org/guix/guix/issues/5899.  Until
                ;; the issue is addressed, I will have to remove
                ;; 'gdm-service-type' from '%desktop-services'.
                (delete gdm-service-type))))
     (bootloader (bootloader-configuration
                   (bootloader grub-efi-bootloader)
                   (targets (list "/boot/efi"))
                   (keyboard-layout keyboard-layout)))

     (file-systems (cons* btrfs-subvolume-home
                          btrfs-subvolume-root
                          btrfs-subvolume-swap
                          partition-efi
                          %base-file-systems))
     ;; Activate swap devices (swapon); swapfiles/partitions must be
     ;; set up manually beforehand
     (swap-devices
      (list swap-space-hibernation
            swap-space-everyday))

     (kernel-arguments
      (append swap-kernel-parameters

              ;; Enable zswap.  For a comparison of zswap to zram,
              ;; read docs/memory_management.org.
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
