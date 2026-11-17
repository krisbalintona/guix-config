(define-module (krisb config machines Maur hardware)
  #:use-module (gnu system file-systems)
  #:use-module (ice-9 filesystem)
  #:export (btrfs-subvolume-root
            btrfs-subvolume-home
            partition-efi

            btrfs-subvolume-swap
            swap-space-hibernation
            swap-space-everyday
            swap-kernel-parameters))


;;
;;; Partition UUIDs
;;

(define efi-partition-uuid "2037-86C8")

(define root-partition-uuid "3a9b954f-6eca-4272-baa2-9d7554d118bc")


;;
;;; Partitions and BTRFS subvolumes
;;
;; NOTE: Mounted BTRFS subvolumes inherit the same flags as the first
;; mounted subvolumes, even if different ones are specified.

(define partition-efi
  (file-system
    (mount-point "/boot/efi")
    (device (uuid efi-partition-uuid 'fat32))
    (type "vfat")))

(define btrfs-subvolume-root
  (file-system
    (mount-point "/")
    (device (uuid root-partition-uuid 'btrfs))
    (type "btrfs")
    (options "subvol=@,compress=zstd")))

(define btrfs-subvolume-home
  (file-system
    (mount-point "/home")
    (device (uuid root-partition-uuid 'btrfs))
    (type "btrfs")
    (options "subvol=@home,compress=zstd")))


;;
;;; Swap spaces
;;
;; For a discussion on swap, its benefits of swap, as well as
;; recommendations, see docs/memory_management.org.

(define swap-path "/swap")

(define btrfs-subvolume-swap
  (file-system
    (mount-point swap-path)
    (device (uuid root-partition-uuid 'btrfs))
    (type "btrfs")
    (options "subvol=@swap,compress=zstd")))

;; Size is a little greater than 2/5 the total RAM
(define swap-space-hibernation ; Used for hibernation
  (swap-space
    (target (file-name-join* swap-path "swapfile-1"))
    (dependencies (list btrfs-subvolume-swap))
    ;; As documented in (guix) Swap Space, a priority of #f (which
    ;; actually ends up as a priority of -1) means this swap space is
    ;; only used once swap spaces of a positive integrer priority are
    ;; used.
    (priority #f)))

;; Size depends on one's observed usage
(define swap-space-everyday ; For everday, non-hibernation paging
  (swap-space
    (target (file-name-join* swap-path "swapfile-2"))
    (dependencies (list btrfs-subvolume-swap))
    (priority 100)))

;; Kernel parameters to use swapfile to resume from hibernation
(define swap-kernel-parameters
  (list (string-append "resume=" root-partition-uuid)
        "resume_offset=11019520"))
