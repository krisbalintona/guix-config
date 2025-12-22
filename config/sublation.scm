(use-modules (gnu)
             (gnu system accounts)
             (gnu packages shells)
             (gnu services shepherd)
             (gnu services desktop)
             (gnu services xorg)
             (nonguix transformations)
             (gnu services containers)
             (gnu services sysctl)
             (gnu services ssh)
             (gnu packages linux)
             (gnu services networking)
             (ice-9 textual-ports)                   ; For 'get-string-all'
             (krisb packages networking)
             (gnu services security)
             (gnu services dns)
             (gnu services vpn)
             (gnu services sysctl))

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
                                           "cgroup")))
                 %base-user-accounts))

   (packages
    (cons*
     
     glibc-locales
     %base-packages))

   ;; Below is the list of system services.  To search for available
   ;; services, run 'guix system search KEYWORD' in a terminal.
   (services
    (cons*
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
     ;; TODO 2025-12-22: Figure out an ergonomic solution to avoid this.
     ;; For rootless podman services (Caddy and Pihole)
     (simple-service 'sysctl-podman
         sysctl-service-type
       '(("net.ipv4.ip_unprivileged_port_start" . "53")))
     (service openssh-service-type)
     (service nftables-service-type
       (nftables-configuration
         (ruleset
          (computed-file
              "nftables-ruleset"
            #~(call-with-output-file #$output
                (lambda (port)
                  (format port
                          #$(call-with-input-file
                                (string-append (dirname (current-filename))
                                               "/files/nftables/sublation.nft")
                              get-string-all)
                          ;; Contains all country definitions/alias, e.g.,
                          ;; $US resolves to a particular number ID
                          (string-append #$nftables-geoip "/etc/nftables/geoip-def-all.nft")
                          ;; Map from IPv4 to country
                          (string-append #$nftables-geoip "/etc/nftables/geoip-ipv4.nft")
                          ;; Map from IPv6 to country
                          (string-append #$nftables-geoip "/etc/nftables/geoip-ipv6.nft"))))))))
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
     ;; I use Unbound with Pihole.  For an explanation of why (and a guide
     ;; on how to do so) I pair Unbound with the latter, see
     ;; https://docs.pi-hole.net/guides/dns/unbound/
     (service unbound-service-type
       (unbound-configuration
         (server
          (unbound-server
            ;; Let only my machine query Unbound (Pihole forwards DNS
            ;; queries to Unbound)
            (interface '("127.0.0.1" ; IPv4 local host
                         "::1"))     ; IPv6 local
            (tls-cert-bundle "/etc/ssl/certs/ca-certificates.crt")
            (hide-version #t)
            (hide-identity #t)
            ;; See
            ;; https://unbound.docs.nlnetlabs.nl/en/latest/manpages/unbound.conf.html
            ;; for a description of all options
            (extra-options
             '((port . 5335) ; Pihole forwards DNS queries to this port
               ;; These are already default, but I declare them explicitly
               ;; anyway
               (do-ip4 . "yes")
               (do-udp . "yes")
               (do-tcp . "yes")
               ;; The following can be set to "yes" if my WAN supports
               ;; IPv6.  I can check by going to test-ipv6.com
               (do-ip6 . "no")
               ;; All paths below are relative to CHROOT, if set.  Make
               ;; sure CHROOT has the permissions of the unbound process.
               ;; The default user of the process is "unbound" (see also
               ;; the USERNAME option).  To change permissions, do
               ;; something like:
               ;;
               ;;     sudo chown unbound:unbound CHROOT_PATH
               ;;
               ;; I do not CHROOT since unbound already runs as a user
               ;; without elevated privileges, and that's good enough for
               ;; me.
               (chroot . "")
               ;; Logging.  If LOGFILE is a path, then ensure its parent
               ;; directory is created and that LOGFILE or its directory is
               ;; owned by "unbound" (the default value of the USERNAME
               ;; option).  If LOGFILE is not set or is set to an empty
               ;; string, then log output to "sudo herd status unbound"
               ;; instead
               (verbosity . "1")  ; Default
               (logfile . "")
               (log-time-ascii . "yes")
               (log-destaddr . "yes")
               (log-queries . "yes")
               (log-servfail . "yes")
               ;; TODO 2025-12-16: Write a service to do this
               ;; automatically.  But first check if there truly is no
               ;; currently existing way to do it automatically.
               ;;
               ;; Use DNSSEC.
               ;;
               ;; NOTE 2025-12-12: On Guix Systems, the root key has to be
               ;; created manually, it seems.  Ensure the parent directory
               ;; of the file exists then run:
               ;;
               ;;     sudo unbound-anchor -a /PATH/TO/KEY/root.key
               ;;
               ;; Unbound also runs as the "unbound" user, so for extra
               ;; security you can change the permissions of the file, too:
               ;;
               ;;     sudo chown unbound:unbound /PATH/TO/KEY
               ;;
               ;; Or, the first command can be ran with "-u unbound", like
               ;; so:
               ;;
               ;;     sudo -u unbound unbound-anchor -a /PATH/TO/KEY/root.key
               ;;
               (auto-trust-anchor-file . "/var/lib/unbound/root.key")
               (harden-glue . "yes")
               (harden-dnssec-stripped . "yes")
               ;; Give minimal domain name information in queries to DNS
               ;; servers.  See also the 'qname-minimisation-strict'
               ;; setting.
               (qname-minimisation . "yes")
               ;; See
               ;; https://docs.pi-hole.net/guides/dns/unbound/#configure-unbound
               ;; for an explanation of these values
               (edns-buffer-size . "1232")
               (so-rcvbuf . "1m")
               (use-caps-for-id . "no")
               (num-threads . "1")
               (prefetch . "yes")))))
         (remote-control
          (unbound-remote
            (control-enable #t)
            ;; Use with:
            ;;
            ;;     sudo unbound-control -s CONTROL-INTERFACE status
            ;;
            (control-interface "/run/unbound.sock"))) ; Default value
         ;; We place the below in EXTRA-CONTENT because we either need to
         ;; unquote a value entirely in the config or quote portions of
         ;; them (which can be done if the cdr is a single Guile symbol).
         ;; But UNBOUND-SERVER always quotes values, so to deal with edge
         ;; cases we place certain settings in EXTRA-CONTENT.  (There can
         ;; be multiple "server:" blocks in the config, it seems.)
         (extra-content
          "server:
             access-control: 127.0.0.0/8 allow
     
             # Ensure privacy of local IP ranges.  Taken from
             # https://docs.pi-hole.net/guides/dns/unbound/#configure-unbound
             private-address: 192.168.0.0/16
             private-address: 169.254.0.0/16
             private-address: 172.16.0.0/12
             private-address: 10.0.0.0/8
             private-address: fd00::/8
             private-address: fe80::/10
     
             # Ensure no reverse queries to non-public IP ranges (RFC6303
             # 4.2).  Taken from
             # https://docs.pi-hole.net/guides/dns/unbound/#configure-unbound
             private-address: 192.0.2.0/24
             private-address: 198.51.100.0/24
             private-address: 203.0.113.0/24
             private-address: 255.255.255.255/32
             private-address: 2001:db8::/32")))
     (service wireguard-service-type
       (wireguard-configuration
         (shepherd-requirement '(nftables))
         (addresses '("10.0.0.1/24"))
         ;; TODO 2025-12-21: Avoid hardcoding this
         (private-key "/run/user/1000/secrets/wireguard-private-key")
         (bootstrap-private-key? #f)
         ;; Network rules
         (pre-up
          (list
           ;; IPv4 and IPv6 table for address translation (rewriting packet
           ;; addresses)
           #~(string-append #$(file-append nftables "/sbin/nft")
                            " add table inet wg-nat")
           #~(string-append #$(file-append nftables "/sbin/nft")
                            " add chain inet wg-nat postrouting '{ type nat hook postrouting priority -100; }'")
           ;; Rewrite source IP of Wireguard outgoing packets (from
           ;; OnePlus) leaving via wifi to be the host's IP
           #~(string-append #$(file-append nftables "/sbin/nft")
                            " add rule inet wg-nat postrouting oifname 'wlp108s0'"
                            " ip saddr 10.0.0.0/24 masquerade")))
         (post-down
          (list
           #~(string-append #$(file-append nftables "/sbin/nft")
                            " delete table inet wg-nat")))
         (peers
          (list
           (wireguard-peer
             (name "OnePlus")
             (public-key "Mgj/EOTOJv96q6NSN8CC7DEXB7ic6WV78H1ukHm7fyY=")
             (allowed-ips '("10.0.0.2/32")))))))
     ;; For Wireguard IP forwarding
     (simple-service 'sysctl-wireguard
         sysctl-service-type
       '(("net.ipv4.ip_forward" . "1")))
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
     %base-services))
   
   (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets (list "/boot/efi"))
                 (keyboard-layout keyboard-layout)))

   (file-systems (cons* (file-system
                          (mount-point "/mnt/backup-hdd")
                          (flags '(no-atime no-diratime))
                          (device (uuid
                                   "7526f253-87d5-47d9-80f9-66c99c70bb8f"
                                   'ext4))
                          (type "ext4")
                          (create-mount-point? #t)
                          (mount-may-fail? #t))
                        (file-system
                          (mount-point "/")
                          (device (uuid
                                   "849da8e9-c001-4a8b-9948-d996c774fc09"
                                   'ext4))
                          (type "ext4"))
                        (file-system
                          (mount-point "/boot/efi")
                          (device (uuid "8D5D-5605"
                                        'fat32))
                          (type "vfat"))
                        %base-file-systems))))
