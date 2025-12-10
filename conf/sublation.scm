(use-modules (gnu)
             (gnu system accounts)
             (gnu packages shells)
             (nonguix transformations)
             (rosenthal services web))
(use-service-modules cups
                     desktop
                     networking
                     ssh
                     xorg
                     containers)

((compose (nonguix-transformation-guix)
          (nonguix-transformation-linux))
 (operating-system
   (locale "en_US.utf8")
   (timezone "America/Chicago")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "sublation")

   ;; The list of user accounts ('root' is implicit).
   (users (cons* (user-account
                   (name "krisbalintona")
                   (comment "Kristoffer Balintona")
                   (group "users")
                   (home-directory "/home/krisbalintona")
                   ;; REVIEW 2025-12-06: Fish shell through tramp has
                   ;; failed despite all my attempts
                   ;; (shell (file-append fish "/bin/fish"))
                   (supplementary-groups '("wheel" "netdev"
                                           "audio" "video"
                                           ;; For rootless Podman
                                           "cgroup")))
                 %base-user-accounts))

   (packages
    (cons glibc-locales
          %base-packages))

   ;; Below is the list of system services.  To search for available
   ;; services, run 'guix system search KEYWORD' in a terminal.
   (services
    (cons* ;; The two services below are for rootless Podman.  See
           ;; (guix) Miscellaneous Services
           (service iptables-service-type)
           (service rootless-podman-service-type
             (rootless-podman-configuration
               (subgids (list (subid-range (name "krisbalintona"))))
               (subuids (list (subid-range (name "krisbalintona"))))))
           (service openssh-service-type)
           (service network-manager-service-type)
           (service wpa-supplicant-service-type)
           (service ntp-service-type)
           (service elogind-service-type
             (elogind-configuration
               (handle-lid-switch 'ignore)
               (handle-lid-switch-docked 'ignore)
               (handle-lid-switch-external-power 'ignore)
               (lid-switch-ignore-inhibited? #t)))
           %base-services))
   
   (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets (list "/boot/efi"))
                 (keyboard-layout keyboard-layout)))

   ;; The list of file systems that get "mounted".  The unique file
   ;; system identifiers there ("UUIDs") can be obtained by running
   ;; 'blkid' in a terminal.
   (file-systems (cons* (file-system
                          (mount-point "/")
                          (device (uuid
                                   "fc895b22-bd9a-4c70-8449-2aed8ed7a116"
                                   'btrfs))
                          (type "btrfs"))
                        (file-system
                          (mount-point "/boot/efi")
                          (device (uuid "8D5D-5605"
                                        'fat32))
                          (type "vfat")) %base-file-systems))))
