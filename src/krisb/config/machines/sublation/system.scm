(define-module (krisb config machines sublation system)
  #:use-module (krisb config common)
  #:use-module (krisb config machines features substitute-servers)
  #:use-module (krisb config machines sublation device)
  #:use-module (krisb config machines sublation hardware)
  #:use-module (gnu)
  #:use-module (gnu system accounts)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu packages shells)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (gnu system file-systems)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module ((krisb config machines sublation device) #:prefix sublation:)
  #:use-module (gnu services containers)
  #:use-module (gnu services sysctl)
  #:use-module (gnu services containers)
  #:use-module (gnu packages linux)
  #:use-module (gnu services networking)
  #:use-module (ice-9 textual-ports)                   ; For 'get-string-all'
  #:use-module (krisb packages networking)
  #:use-module (gnu services security)
  #:use-module (krisb packages netbird)
  #:use-module (krisb services netbird)
  #:use-module (gnu services messaging)
  #:use-module (gnu services monitoring)
  #:use-module (gnu services monitoring))



(define-public sublation-operating-system
  (operating-system
    ;; Nonguix kernel and CPU microcode. See
    ;; https://gitlab.com/nonguix/nonguix#using-nonfree-firmware-and-drivers
    ;; and https://gitlab.com/nonguix/nonguix#cpu-microcode for
    ;; instructions.
    ;;
    ;; See also the (nonguix transformations) module from the nonguix
    ;; channel for several convenience wrappers (e.g.,
    ;; nonguix-transformation-guix, nonguix-transformation-linux,
    ;; nonguix-transformation-nvidia)
    (kernel linux)
    (firmware (list linux-firmware))
    (initrd microcode-initrd)

    (kernel-arguments
     (append
      ;; Enable zswap.  For a comparison of zswap to zram, read
      ;; docs/memory_management.org.
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
      ;; Enable the kernel module corresponding to step 2 in
      ;; https://jellyfin.org/docs/general/post-install/transcoding/hardware-acceleration/intel/#configure-and-verify-lp-mode-on-linux
      (list "i915.enable_guc=3")
      %default-kernel-arguments))

    (locale "en_US.utf8")
    (timezone "America/Chicago")
    (keyboard-layout (keyboard-layout "us"))
    (host-name "sublation")
    
    ;; The list of user accounts ('root' is implicit).
    (users (cons* (user-account
                    (name "krisbalintona")
                    (comment "Kristoffer Balintona")
                    (home-directory "/home/krisbalintona")
                    (shell (file-append fish "/bin/fish"))
                    (group "users")
                    (supplementary-groups '("wheel" "netdev"
                                            "audio" "video"
                                            "cgroup"
                                            "containers")))
                  %base-user-accounts))

    (groups (cons*
	     (user-group
	       (name "containers")
	       (id 166535))
	     %base-groups))

    (packages
     (cons*
      netbird
      glibc-locales
      %base-packages))

    ;; Below is the list of system services.  To search for available
    ;; services, run 'guix system search KEYWORD' in a terminal.
    (services
     (append (feature-substitute-servers)
             common-system-services
             (cons*
              (service sops-secrets-service-type
                (sops-service-configuration
                  (age-key-file sublation:%sops-age-key-file)
                  (verbose? #t)
                  (secrets
                   (list
                    ))))
              ;; Rootless podman also needs 'iptables-service-type' specifically for
              ;; its (non-internal) networks; 'nftables-service-type' will not
              ;; suffice.  See (guix) Miscellaneous Services.  We may have both,
              ;; though, since the default ruleset for iptables is to accept
              ;; everything.
              (service iptables-service-type)
              (service rootless-podman-service-type
                (rootless-podman-configuration
                  (subgids (list (subid-range (name "krisbalintona"))))
                  (subuids (list (subid-range (name "krisbalintona"))))))
              ;; TODO 2025-12-22: Figure out an ergonomic solution to avoid lowering
              ;; the unprivileged port start?
              ;;
              ;; For rootless services that require lower port numbers, such as a
              ;; reverse proxy (Caddy) and DNS server (Technitium)
              (simple-service 'sysctl-podman
                  sysctl-service-type
                '(("net.ipv4.ip_unprivileged_port_start" . "53")))
              (service nftables-service-type
                (nftables-configuration
                  (ruleset
                   (local-file (config-files-path "nftables/sublation.nft")))))
              ;; Enable IPv4 packet forwarding
              ;;
              ;; NOTE: Plain Wireguard doesn't do this for me, but Netbird does
              ;; (according to
              ;; https://docs.netbird.io/manage/networks/how-routing-peers-work#ip-forwarding).
              ;; Nevertheless, we set this in case IPv4 forwarding is possible.
              ;; Also be aware of its IPv6 counterpart:
              ;; net.ipv6.conf.all.forwarding.
              (simple-service 'sysctl-nftables-forwarding
                  sysctl-service-type
                '(("net.ipv4.ip_forward" . "1")))
              (service fail2ban-service-type)
              (simple-service 'fail2ban-openssh
                  fail2ban-service-type
                (list
                 (fail2ban-jail-configuration
                   (name "sshd")
                   (enabled? #t)
                   (max-retry 5)
                   (find-time "10m")
                   (ban-time "1h"))))
              (simple-service 'fail2ban-caddy
                  fail2ban-service-type
                (list 
                 (fail2ban-jail-configuration
                   (name "caddy-bots")
                   (enabled? #t)
                   (max-retry 3)
                   (find-time "5m")
                   (ban-time "1h")
                   (log-path
                    '("/home/krisbalintona/services/caddy/log/copyparty.log"))
                   (filter
                    (fail2ban-jail-filter-configuration
                      (name "nginx-botsearch"))))))
              (service netbird-service-type
                (netbird-configuration
                 (respawn? #f)))
              (service soju-service-type
                (soju-configuration
                 (title "Personal bouncer")
                 (hostname "irc.home.kristofferbalintona.me")
                 (listen '("irc://localhost:6667"
                           "http://localhost:9500"
                           ;; Create a UNIX socket allows administering the bouncer
                           ;; from the command line with `sojuctl'.  (Recommended in
                           ;; (guix) Messaging Services.)
                           "unix+admin:///var/lib/soju/soju.sock"))
                 (extra-content
                  ;; I use Caddy with the caddy-l4 (layer 4) app as a reverse proxy
                  ;; in front of Soju which handles TLS termination.
                  ;; "accept-proxy-ip localhost" is needed to tell Soju to trust the
                  ;; PROXY protocol header that I've configured caddy-l4 to wrap the
                  ;; TCP packets in ("proxy_protocol v2").
                  (list (plain-file "soju-trusted-proxy.conf" "accept-proxy-ip localhost")))))
              (service prometheus-node-exporter-service-type
                (prometheus-node-exporter-configuration
                  (web-listen-address "127.0.0.1:21005")))
              (simple-service 'mergerfs-pools shepherd-root-service-type
                (list shepherd-service-mergerfs-media
                      shepherd-service-mergerfs-torrents-incomplete
                      shepherd-service-mergerfs-immich))
              (service network-manager-service-type)
              (service wpa-supplicant-service-type)
              (service ntp-service-type)
              (service elogind-service-type
                (elogind-configuration
                  ;; Inhibit laptop sleeping and hibernation on lid close
                  (handle-lid-switch 'ignore)
                  (handle-lid-switch-docked 'ignore)
                  (handle-lid-switch-external-power 'ignore)
                  (lid-switch-ignore-inhibited? #t)))
              %base-services)))
    
    (bootloader (bootloader-configuration
                  (bootloader grub-efi-bootloader)
                  (targets (list "/boot/efi"))
                  (keyboard-layout keyboard-layout)))

    (file-systems (cons* backup-partition
                         system-partition-root
                         system-partition-efi
                         ;; HDD 1
                         file-system-hdd-1-media
                         file-system-hdd-1-torrents-incomplete
                         file-system-hdd-1-immich
                         ;; HDD 2
                         file-system-hdd-2-media
                         file-system-hdd-2-torrents-incomplete
                         file-system-hdd-2-immich
                         %base-file-systems))

    (swap-devices
     (list swap-space-swapfile))))
