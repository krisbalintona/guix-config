(define-module (krisb config machines sublation hardware)
  #:use-module (krisb config common)
  #:use-module (gnu system file-systems)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages file-systems)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-13)
  )

(define media-mount-point "/home/krisbalintona/services/media")
(define mergerfs-mount-options-base
  (string-join
   '(;; FIXME 2026-07-01: We set "allow_other" (here and in other
     ;; pools) only because I'm currently on MergerFS version 2.33.5.
     ;; According to the MergerFS documentation
     ;; (https://trapexit.github.io/mergerfs/latest/config/deprecated_options/),
     ;; MergerFS version 2.35.0 and above sets this automatically
     ;; (when running as root).  Remove this mount option when
     ;; MergerFS is upgraded to this version or above.
     "allow_other"                ; A FUSE option, not MergerFS option
     ;; Some media-related services (e.g., Jellyfin) depend on mtimes
     ;; for e.g., determining when to do scans.
     "func.getattr=newest"
     ;; Ensure inode numbers are consistent.
     ;;
     ;; FIXME 2026-07-02: Currently use MergerFS version 2.33.5 (from
     ;; 2022).  However, according to
     ;; https://trapexit.github.io/mergerfs/latest/config/inodecalc/,
     ;; v2.35.0+ no longer needs this option.  When I upgrade to such
     ;; a version, remove this mount point here and elsewhere.
     "use_ino"
     ;; Recommended as per "man mergerfs".  (For instance, Bittorrent
     ;; clients may use mmap.)
     ;;
     ;; FIXME 2026-07-02: When I upgrade MergerFS versions check if
     ;; these are still the recommendations.
     "cache.files=partial"
     "dropcacheonclose=true"
     ;;   Just use another drive when one gets full (actually or by
     ;;   disk quota) mid-write.
     "moveonenospc=mfs")
   ","))
;; HDD 1
(define hdd-1-uuid "9ebe0061-06bd-477d-b32b-5deeda8b757c")

(define hdd-1-mount-point "/mnt/media-hdd1")

(define hdd-1-mount-point-media (string-append hdd-1-mount-point "/media"))

(define hdd-1-mount-point-torrents-incomplete
  (string-append hdd-1-mount-point "/torrents-incomplete"))

(define hdd-1-mount-point-immich (string-append hdd-1-mount-point "/immich"))

;; HDD 2
(define hdd-2-uuid "949d6370-7963-44cf-8668-53a3fe81e06c")

(define hdd-2-mount-point "/mnt/media-hdd2")

(define hdd-2-mount-point-media (string-append hdd-2-mount-point "/media"))

(define hdd-2-mount-point-torrents-incomplete
  (string-append hdd-2-mount-point "/torrents-incomplete"))

(define hdd-2-mount-point-immich (string-append hdd-2-mount-point "/immich"))

;; HDD 1
(define-public file-system-hdd-1-media
  (file-system
    (device (uuid hdd-1-uuid))
    (mount-point hdd-1-mount-point-media)
    (type "btrfs")
    (flags '(no-atime))
    (options "subvol=@media,compress=zstd")
    (create-mount-point? #t)))

(define-public file-system-hdd-1-torrents-incomplete
  (file-system
    (device (uuid hdd-1-uuid))
    (mount-point hdd-1-mount-point-torrents-incomplete)
    (type "btrfs")
    (flags '(no-atime))
    ;; This subvolume is not mounted with "nodatacow," but it consists
    ;; of one directory that is NOCOW.  I avoid setting NOCOW via
    ;; mount option because btrfs subvolumes currently inherit the
    ;; mount options of the first mounted subvolume.  To avoid
    ;; mistakes due to failing to maintain mounting order, I avoid
    ;; relying on the "nodatacow" mount option.
    (options "subvol=@torrents-incomplete,compress=zstd")
    (create-mount-point? #t)))

(define-public file-system-hdd-1-immich
  (file-system
    (device (uuid hdd-1-uuid))
    (mount-point hdd-1-mount-point-immich)
    (type "btrfs")
    (flags '(no-atime))
    (options "subvol=@immich,compress=zstd")
    (create-mount-point? #t)))

;; HDD 2
(define-public file-system-hdd-2-media
  (file-system
    (device (uuid hdd-2-uuid))
    (mount-point hdd-2-mount-point-media)
    (type "btrfs")
    (flags '(no-atime))
    (options "subvol=@media,compress=zstd")
    (create-mount-point? #t)))

(define-public file-system-hdd-2-torrents-incomplete
  (file-system
    (device (uuid hdd-2-uuid))
    (mount-point hdd-2-mount-point-torrents-incomplete)
    (type "btrfs")
    (flags '(no-atime))
    ;; This subvolume is not mounted with "nodatacow."  For why, see
    ;; the comment somewhere above.
    (options "subvol=@torrents-incomplete,compress=zstd")
    (create-mount-point? #t)))

(define-public file-system-hdd-2-immich
  (file-system
    (device (uuid hdd-2-uuid))
    (mount-point hdd-2-mount-point-immich)
    (type "btrfs")
    (flags '(no-atime))
    (options "subvol=@immich,compress=zstd")
    (create-mount-point? #t)))

(define-public mergerfs-pool-media
  (file-system
    (device
     (string-append hdd-1-mount-point-media ":" hdd-2-mount-point-media))
    (mount-point media-mount-point)
    (type "fuse.mergerfs")
    (options
     (string-append mergerfs-mount-options-base
                    ;; TODO 2026-07-02: Consider switching to the
                    ;; "pfrd" policy once I upgrade to a version that
                    ;; offers it.
                    ",category.create=mfs,minfreespace=10G"))
    (dependencies (list file-system-hdd-1-media file-system-hdd-2-media))))

(define-public mergerfs-pool-torrents-incomplete
  (file-system
    (device
     (string-append hdd-1-mount-point-torrents-incomplete ":" hdd-2-mount-point-torrents-incomplete))
    (mount-point (string-append media-mount-point "/downloads/bittorrent/incomplete"))
    (type "fuse.mergerfs")
    (options
     (string-append mergerfs-mount-options-base
                    ;; The "epmfs" (existing path, most free space)
                    ;; policy is used for torrents because torrent
                    ;; clients expect a stable directory structure and
                    ;; should not have files scattered across
                    ;; different MergerFS branches for the same path.
                    ;; This ensures downloads for a given torrent stay
                    ;; on a single disk while still allowing new
                    ;; torrents to be distributed across the pool
                    ;; based on available space, reducing
                    ;; fragmentation and avoiding cross-disk writes.
                    ",category.create=mfs,minfreespace=10G"))
    (dependencies
     (list file-system-hdd-1-torrents-incomplete
           file-system-hdd-2-torrents-incomplete
           mergerfs-pool-media))))
(define-public mergerfs-pool-immich
  (file-system
    (device
     (string-append hdd-1-mount-point-immich ":" hdd-2-mount-point-immich))
    (mount-point "/home/krisbalintona/services/immich/data")
    (type "fuse.mergerfs")
    (options
     (string-append mergerfs-mount-options-base
                    ",category.create=mfs,minfreespace=10G"))
    (dependencies (list file-system-hdd-1-immich file-system-hdd-2-immich))))
;;; Mergerfs pools
;;
;; Guix's 'file-system' record only accepts a device that is a label,
;; a UUID, or a /dev node (see (guix) File Systems).  Mergerfs's
;; colon-separated branch syntax ("path1:path2") doesn't fit any of
;; those, and Guix's device-resolution code (canonicalize-device-spec)
;; has no case for it, so these pools cannot be declared as
;; 'file-system' entries no matter what 'device'/'type'/'options' are
;; set to.  Instead, each pool is started directly as an ordinary
;; Shepherd service that execs the 'mergerfs' binary, the same way
;; Guix itself starts any other daemon.

(define* (mergerfs-shepherd-service name branches mount-point mergerfs-options
                                    #:optional (extra-requirement '()))
  "Return a shepherd-service that mounts BRANCHES as a MergerFS pool.
BRANCHES is a list of directory name strings at MOUNT-POINT via
MergerFS.  It is mounted with MERGERFS-OPTIONS (a comma-separated
string denoting mount options).

EXTRA-REQUIREMENT is an extra list of Shepherd provision symbols to
depend on, e.g., another MergerFS pool that MOUNT-POINT happens to
live underneath."
  (with-imported-modules '((guix build utils))
    (shepherd-service
      (provision (list (symbol-append 'mergerfs- name)))
      (requirement
       (append
        ;; Wait for each underlying btrfs branch mount.  Guix names
        ;; the Shepherd service for a 'file-system' entry
        ;; 'file-system-<mount-point>'.
        (map (lambda (branch)
               (symbol-append 'file-system- (string->symbol branch)))
             branches)
        extra-requirement))
      (documentation
       (string-append "Create a MergerFS pool and mount it at " mount-point "."))
      (start
       #~(lambda _
           (mkdir-p #$mount-point)
           (let ((command
                  (list #$(file-append mergerfs "/bin/mergerfs")
                        "-f"              ; Stay in foreground for Shepherd
                        "-o" #$mergerfs-options
                        #$(string-join branches ":")
                        #$mount-point)))
             (format #t "mergerfs command to run: ~a~%" command)
             (let ((pid ((make-forkexec-constructor command))))
               (format #t "Spawned mergerfs (pid ~a) for pool at ~a~%"
                       pid mount-point)
               pid))))
      (stop #~(make-kill-destructor)))))

(define-public shepherd-service-mergerfs-media
  (mergerfs-shepherd-service
   'media
   (list hdd-1-mount-point-media hdd-2-mount-point-media)
   media-mount-point
   ;; 2026-07-02: Manually copied from 'mergerfs-pool-media'
   (string-append mergerfs-mount-options-base
                  ",category.create=mfs,minfreespace=10G")))

(define-public shepherd-service-mergerfs-torrents-incomplete
  (mergerfs-shepherd-service
   'torrents-incomplete
   (list hdd-1-mount-point-torrents-incomplete
         hdd-2-mount-point-torrents-incomplete)
   (string-append media-mount-point "/downloads/bittorrent/incomplete")
   ;; 2026-07-02: Manually copied from 'mergerfs-pool-media'
   (string-append mergerfs-mount-options-base
                  ",category.create=mfs,minfreespace=10G")
   ;; This mount point lives inside the shepherd service created in
   ;; 'shepherd-service-mergerfs-media' so it must come up only after
   ;; the media pool is already mounted.
   '(mergerfs-media)))

(define-public shepherd-service-mergerfs-immich
  (mergerfs-shepherd-service
   'immich
   (list hdd-1-mount-point-immich hdd-2-mount-point-immich)
   "/home/krisbalintona/services/immich/data"
   ;; 2026-07-02: Manually copied from 'mergerfs-pool-media'
   (string-append mergerfs-mount-options-base
                  ",category.create=mfs,minfreespace=10G")))
