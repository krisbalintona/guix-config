(use-modules (gnu)
             (gnu system accounts)
             (gnu packages shells)
             (nonguix transformations)
             (rosenthal services web)
             (krisb packages networking)
             (ice-9 textual-ports))     ; For 'get-string-all'
(use-service-modules shepherd
                     cups
                     desktop
                     networking
                     ssh
                     xorg
                     containers
                     security
                     sysctl
                     dns)

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
    (cons* (service fail2ban-service-type
             (fail2ban-configuration
               (extra-jails
                (list
                 ;; SSH jail
                 (fail2ban-jail-configuration
                   (name "sshd")
                   (enabled? #t)
                   (max-retry 5)
                   (find-time "10m")
                   (ban-time "1h"))
                 ;; Caddy jail
                 (fail2ban-jail-configuration
                   (name "caddy-bots")
                   (enabled? #t)
                   (max-retry 3)
                   (find-time "5m")
                   (ban-time "1h")
                   ;; TODO 2025-12-10: Can we shorten this path and
                   ;; compute it dynamically?  This depends on the
                   ;; internal volume created by Podman for my Caddy
                   ;; service
                   (log-path
                    '("/home/krisbalintona/.local/share/containers/storage/volumes/caddy_log/_data/caddy/access.log"))
                   (filter
                    (fail2ban-jail-filter-configuration
                      (name "nginx-botsearch"))))))))
           ;; Rootless podman also needs 'iptables-service-type'
           ;; specifically for its (non-internal) networks;
           ;; 'nftables-service-type' will not suffice.  See (guix)
           ;; Miscellaneous Services.  We may have both, though, since
           ;; the default ruleset for iptables is to accept
           ;; everything.
           (service iptables-service-type)
           (service rootless-podman-service-type
             (rootless-podman-configuration
               (subgids (list (subid-range (name "krisbalintona"))))
               (subuids (list (subid-range (name "krisbalintona"))))))
           (service unbound-service-type
             (unbound-configuration
               (server
                (unbound-server
                  ;; Listen on all interfaces, IPv4, IPv6, and the
                  ;; local subnet
                  (interface '("0.0.0.0" "::0"))
                  (tls-cert-bundle "/etc/ssl/certs/ca-certificates.crt")
                  (hide-version #t)
                  (hide-identity #t)
                  ;; See
                  ;; https://unbound.docs.nlnetlabs.nl/en/latest/manpages/unbound.conf.html
                  ;; for a description of all options
                  (extra-options
                   '(;; These are already default, but I declare them
                     ;; explicitly anyway
                     (do-ip4 . "yes")
                     (do-udp . "yes")
                     (do-tcp . "yes")
                     ;; All paths below are relative to CHROOT, if
                     ;; set.  Make sure CHROOT has the permissions of
                     ;; the unbound process.  The default user of the
                     ;; process is "unbound" (see also the USERNAME
                     ;; option).  To change permissions, do something
                     ;; like:
                     ;;
                     ;;     sudo chown unbound:unbound CHROOT_PATH
                     ;;
                     ;; I do not CHROOT since unbound already runs as
                     ;; a user without elevated privileges, and that's
                     ;; good enough for me.
                     (chroot . "")
                     ;; Logging.  If LOGFILE is a path, output to
                     ;; stderr (which sudo herd status shows).
                     ;; Otherwise, output logs to LOGFILE
                     (logfile . "")
                     (use-syslog . "no")
                     (verbosity . "1")  ; Default
                     (log-time-ascii . "yes")
                     (log-destaddr . "yes")
                     (log-queries . "yes")
                     (log-servfail . "yes")
                     ;; TODO 2025-12-16: Write a service to do this
                     ;; automatically.  But first check if there truly
                     ;; is no currently existing way to do it
                     ;; automatically.
                     ;;
                     ;; Use DNSSEC.
                     ;;
                     ;; NOTE 2025-12-12: On Guix Systems, the root key
                     ;; has to be created manually, it seems.  Ensure
                     ;; the parent directory of the file exists then
                     ;; run:
                     ;;
                     ;;     sudo unbound-anchor -a /PATH/TO/KEY/root.key
                     ;;
                     ;; Unbound also runs as the "unbound" user, so
                     ;; for extra security you can change the
                     ;; permissions of the file, too:
                     ;;
                     ;;     sudo chown unbound:unbound /PATH/TO/KEY
                     ;;
                     ;; Or, the first command can be ran with "-u
                     ;; unbound", like so:
                     ;;
                     ;;     sudo -u unbound unbound-anchor -a /PATH/TO/KEY/root.key
                     ;;
                     (auto-trust-anchor-file . "/var/lib/unbound/root.key")
                     (harden-glue . "yes")
                     (harden-dnssec-stripped . "yes")
                     ;; Give minimal domain name information in
                     ;; queries to DNS servers.  See also the
                     ;; 'qname-minimisation-strict' setting.
                     (qname-minimisation . "yes")
                     ;; Performance and security.  An article that
                     ;; shares a lot of info and tips on the matter:
                     ;; https://calomel.org/unbound_dns.html
                     (use-caps-for-id . "yes")
                     (prefetch . "yes")
                     (num-threads . "2")))))
               (remote-control
                (unbound-remote
                  (control-enable #t)
                  ;; Use with:
                  ;;
                  ;;     sudo unbound-control -s CONTROL-INTERFACE status
                  ;;
                  (control-interface "/run/unbound.sock"))) ; Default value
               ;; We place the below in EXTRA-CONTENT because we
               ;; either need to unquote a value entirely in the
               ;; config or quote portions of them (which can be done
               ;; if the cdr is a single Guile symbol).  But
               ;; UNBOUND-SERVER always quotes values, so to deal with
               ;; edge cases we place certain settings in
               ;; EXTRA-CONTENT.  (There can be multiple "server:"
               ;; blocks in the config, it seems.)
               (extra-content
                ;; TODO 2025-12-12: Figure out a way to dynamically
                ;; determine my IP address and subnet
                "server:
        access-control: 127.0.0.0/8 allow
        access-control: 192.168.4.0/22 allow
        local-zone: \"home.arpa.\" static
        local-data: \"sublation.home.arpa. IN A 192.168.4.242\"
        local-data: \"party.home.arpa. IN A 192.168.4.242\"
        local-data: \"goaccess.home.arpa. IN A 192.168.4.242\"")))
           (service openssh-service-type)
           (simple-service 'extend-sysctl
               sysctl-service-type
             '(("net.ipv4.ip_unprivileged_port_start" . "80"))) ; For Caddy
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
                                ;; Contains all country
                                ;; definitions/alias, e.g., $US resolves
                                ;; to a particular number ID
                                (string-append #$nftables-geoip "/etc/nftables/geoip-def-all.nft")
                                ;; Map from IPv4 to country
                                (string-append #$nftables-geoip "/etc/nftables/geoip-ipv4.nft")
                                ;; Map from IPv6 to country
                                (string-append #$nftables-geoip "/etc/nftables/geoip-ipv6.nft"))))))))
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
