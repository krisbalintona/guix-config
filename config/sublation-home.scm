(use-modules (guix gexp)
             (gnu packages)
             (gnu system shadow)        ; For user-group 
             (gnu home services)
             (gnu home services shepherd)
             (gnu home services shells)
             (gnu packages shells)
             (gnu home services shells)
             (abbe packages rust)
             (krisb services shells)
             (abbe packages rust)
             (gnu packages terminals)                ; fzf
             (gnu packages rust-apps)                ; fd
             (krisb services shells)
             (gnu home services dotfiles)
             (gnu packages gnupg)
             (gnu home services gnupg)
             (sops secrets)
             (sops home services sops)
             (gnu packages containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu home services ssh)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (krisb packages networking)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services shepherd)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (krisb services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services containers)
             (gnu home services containers)
             (gnu services backup)
             (gnu home services backup))

(define sops-sublation-secrets-file
  (string-append (dirname (current-filename)) "/files/sops/sublation.yaml"))

;; Get secrets as strings.  Taken from
;; https://github.com/fishinthecalculator/sops-guix/issues/2
(use-modules ((ice-9 popen) #:select (open-input-pipe close-pipe))
             ((rnrs io ports) #:select (get-string-all))
             ((sops secrets) #:select (sanitize-sops-key)))

(define* (get-sops-secret key #:key file (number? #f))
  (let* ((cmd (format #f "sops --decrypt --extract '~a' '~a'"
                      (sanitize-sops-key key)
                      file))
         (port (open-input-pipe cmd))
         (secret (get-string-all port)))
    (close-pipe port)
    (if number?
        (string->number secret)
        secret)))

;; Helper for getting file path of secret
(define (get-sops-secret-path filename)
  (string-append "/run/user/" (number->string (getuid))
                 "/secrets/" filename))
(define home-podman-socket
  (string-append (getenv "XDG_RUNTIME_DIR")
                 "/podman/podman.sock"))
(define services-dir
  (string-append (getenv "HOME") "/services"))
(define pocket-id-socket-dir
    (string-append (getenv "XDG_RUNTIME_DIR")
                   "/pocket-id"))

(define pocket-id-socket
  (string-append pocket-id-socket-dir "/pocket-id.sock"))
(define copyparty-socket-dir
  (string-append (getenv "XDG_RUNTIME_DIR")
                 "/copyparty"))

(define copyparty-socket
  (string-append copyparty-socket-dir "/copyparty.sock"))
(define* (restic-job/defaults
          #:key
          (restic (@ (abbe packages golang) restic)) ; More up-to-date Restic
          name
          (repository "/mnt/backup-hdd")
          (password-file
           ;; FIXME 2025-12-22: This is evaluated at Guix evaluation
           ;; time, not at runtime.  If sops-guix ever changes its
           ;; secret path, this could drift.  This is currently the
           ;; most idiomatic solution without service-level
           ;; integration with sops-guix.
           (string-append "/run/user/"
                          (number->string (getuid))
                          "/secrets/restic-backup-password"))
          files
          schedule
          (wait-for-termination? #t)
          (extra-flags '("--retry-lock" "15m"))
          (verbose? #t))
  (restic-backup-job
    (restic restic)
    (name name)
    (files files)
    (schedule schedule)
    (repository repository)
    (password-file password-file)
    (wait-for-termination? wait-for-termination?)
    (extra-flags extra-flags)
    (verbose? verbose?)))

(home-environment
  (packages
   (specifications->packages
    (list
     ;; Editors
     "vim"
     "neovim"
     "emacs"
     ;; Other packages
     "btop"
     "glances"
     "nmon"
     "atop"
     "git"
     "make"
     "cmake"
     "python"
     "tree"
     "ripgrep"
     "fd"
     "jq"
     "rsync"
     "parted"
     "bat"
     "procs"
     "brightnessctl"
     "jujutsu"
     "fzf"
     "fd"
     "ffmpeg"                          ; Diagnose video info (e.g., codecs)
     "gnupg"
     "age"
     "bind:utils"
     "curl"
     "wget"
     ;; Port scanning
     "nmap"
     "masscan"
     "unbound"
     "caddy-security-netlify-crowdsec-coraza-maxmind"
     "restic"
     "smartmontools"            ; For smartctl
     "btrfs-progs"
     "compsize"
     "lsof")))
  
  (services
   (cons*
    (simple-service 'common-environment-variables
        home-environment-variables-service-type
      '(("PATH" . "$HOME/.local/bin:$PATH")
        ("PAGER" . "less -RKF")))
    (service home-bash-service-type
      (home-bash-configuration
        (bashrc
         (list
          ;; 2025-12-06: My default shell is bash because I haven't gotten
          ;; Tramp to work with Fish shell yet.  (I've deduced that the
          ;; prompt is not the problem.)  So, instead, I keep bash as my
          ;; shell and dispatch to fish if the current process wasn't
          ;; started by tramp
          (plain-file "to-fish.bash"
            "if [[ $- == *i* ]] && { [[ ! $TERM =~ dumb ]] || [[ $INSIDE_EMACS == *,eat* ]]; }; then
        SHELL=$(command -v fish) exec fish
    fi")))))
    (service home-fish-service-type
      (home-fish-configuration
        (config
         (list (plain-file "fish_greeting.fish" "set -g fish_greeting")))
        (environment-variables
         `(("SHELL" . ,(file-append fish "/bin/fish"))))
        (abbreviations '(("cd" . "z")))))
    (simple-service 'home-fish-bat
        home-fish-service-type
      (home-fish-extension
        (aliases
         `(("cat" . ,(string-join '("bat" "--theme=ansi"
                                    "--style=plain,header-filesize,grid,snip --paging auto"
                                    "--italic-text=always --nonprintable-notation=caret")))))))
    (simple-service 'pager-environment-variables
        home-environment-variables-service-type
      ;; Use bat as a pager for man.  Taken from
      ;; https://github.com/sharkdp/bat?tab=readme-ov-file#man
      '(("MANPAGER" . "sh -c 'sed -u -e \"s/\\x1B\\[[0-9;]*m//g; s/.\\x08//g\" | bat -p -lman'")))
    (service home-zoxide-service-type
      (home-zoxide-configuration
        (zoxide (@ (abbe packages rust) zoxide))))
    (simple-service 'home-fish-procs
        home-fish-service-type
      (home-fish-extension
        (abbreviations `(("ps" . "procs")))))
    (simple-service 'krisb-symlink-jj-config-files-service-type
          home-xdg-configuration-files-service-type
        `(("jj/config.toml"
           ,(local-file "files/jujutsu/config.toml"))))
    (simple-service 'fish-vcs-jj
          home-xdg-configuration-files-service-type
        `(("fish/functions/fish_jj_prompt.fish"
           ,(local-file "files/jujutsu/fish_jj_prompt.fish"))
          ("fish/functions/fish_vcs_prompt.fish"
           ,(local-file "files/jujutsu/fish_vcs_prompt.fish"))))
    (simple-service 'fish-fzf-packages
        home-profile-service-type
      (list fd fzf))                        ; Function dependencies
    (simple-service 'fish-fzf-function
          home-xdg-configuration-files-service-type
        `(("fish/functions/fzf_complete.fish"
           ,(local-file "files/fish/fzf_complete.fish"))))
    (simple-service 'fish-fzf-config
        home-fish-service-type
      (home-fish-extension
        (config
         (list (plain-file "fzf_custom.fish" "bind \\t fzf_complete")))))
    (service home-atuin-service-type
      (home-atuin-configuration
        (atuin-fish-flags '("--disable-up-arrow"))
        (atuin-bash-flags '("--disable-up-arrow"))))
    (simple-service 'krisb-symlink-atuin-config-files-service-type
        home-xdg-configuration-files-service-type
      `(("atuin/config.toml"
         ,(local-file "files/atuin/config.toml"))))
    (service home-dotfiles-service-type
      (home-dotfiles-configuration
        (layout 'plain)
        (directories '("files/scripts"))))
    (service home-gpg-agent-service-type)
    (service home-sops-secrets-service-type
      (home-sops-service-configuration
        (config (local-file "files/sops/sops.yaml" "sops.yaml"))
        (secrets
         (list
          (sops-secret
            (key '("pocket-id-encryption-key"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))
          (sops-secret
            (key '("pihole-webserver-password"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))
          (sops-secret
            (key '("caddy" "netlify-access-token"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))
          (sops-secret
            (key '("caddy" "pocket-id" "client-id"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))
          (sops-secret
            (key '("caddy" "pocket-id" "client-secret"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))
          (sops-secret
            (key '("caddy" "crowdsec-bouncer" "api-key"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))
          (sops-secret
            (key '("wireguard-private-key"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))
          (sops-secret
            (key '("vaultwarden" "push-installation-id"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))
          (sops-secret
            (key '("vaultwarden" "push-installation-key"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))
          (sops-secret
            (key '("gluetun" "wireguard_private_key"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))
          (sops-secret
            (key '("gluetun" "wireguard_preshared_key"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))
          (sops-secret
            (key '("restic-backup-password"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))))))
    (service home-oci-service-type
      (for-home
       (oci-configuration
        (runtime 'podman)                   ; Use podman instead of docker
        (verbose? #t))))
    (simple-service 'podman-socket
        home-shepherd-service-type
      (list
       (shepherd-service
         (provision '(home-podman-socket))
         (documentation
          "Run 'podman system service --time 0', creating a podman socket at $XDG_RUNTIME_DIR.")
         (start
          #~(make-forkexec-constructor
             (list #$(file-append podman "/bin/podman")
                   "system" "service" "--time" "0")))
         (stop
          #~(make-kill-destructor)))))
    (simple-service 'home-oci-prometheus-podman-exporter
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (let ((container-podman-socket "/run/podman/podman.sock"))
           (oci-container-configuration
             (provision "prometheus-podman-exporter")
             (requirement '(home-podman-socket))
             (image "quay.io/navidys/prometheus-podman-exporter:latest")
             (environment
              (list
               (cons "CONTAINER_HOST"
                     (string-append "unix://" container-podman-socket))))
             (volumes
              (list (cons home-podman-socket container-podman-socket)))
             ;; See also
             ;; https://github.com/containers/prometheus-podman-exporter?tab=readme-ov-file#usage-and-options
             ;; for a list of other collectors available for enabling.  You
             ;; can see which collectors are enabled from the startup logs
             ;; of the container.  The default collector
             ;; (collector.container) seems sufficient for all the data
             ;; required by the podman-exporter Grafana dashboard I use:
             ;; https://grafana.com/grafana/dashboards/21559-podman-exporter-dashboard/.
             (extra-arguments
              '(;; 2026-01-24: Instructed to include this if, I think, Using
                ;; SELinux, which I'm not.  But I've left it here in the
                ;; future in case I use SELinux.
                "--security-opt" "label=disable"
                ;; The podman socket is only readable by the host user, and
                ;; we must make that socket readable by the container
                ;; process
                "--userns=keep-id:uid=65534"))
             (ports '("127.0.0.1:9882:9882"))
             (auto-start? #t)
             (respawn? #f)))))))
    (service home-openssh-service-type
        (home-openssh-configuration
          (hosts
           (list
            (openssh-host (name "WindowsG14")
                          (host-name "192.168.4.138")
                          (user "krisbalintona")
                          (forward-x11? #t)
                          (forward-x11-trusted? #t))))))
    (simple-service 'home-oci-crowdsec
        home-oci-service-type
      (oci-extension
       (networks
        (list
         (oci-network-configuration
          ;; Should be an external network because CrowdSec requires
          ;; host-outbound internet requests
          (name "crowdsec-network"))))
       (containers
        (list
         (oci-container-configuration
           (provision "crowdsec")
           (image "crowdsecurity/crowdsec:latest")
           (environment
            (list '("TZ" . "America/Chicago")
                  '("LOCAL_API_URL" . "http://127.0.0.1:7200")
                  (string-append "'"
                                 "COLLECTIONS="
                                 (string-join '("crowdsecurity/linux"
                                                "crowdsecurity/sshd"
                                                "crowdsecurity/whitelist-good-actors"
                                                "crowdsecurity/base-http-scenarios"
                                                "crowdsecurity/caddy")
                                              " ")
                                 "'")
                  ;; Enable Write-Ahead Logging with SQLite.  Disable if
                  ;; using a filesystem over the network, e.g., NAS
                  '("USE_WAL" . "true")
                  ;; Bouncers
                  (cons "BOUNCER_KEY_caddy"
                        (get-sops-secret '("caddy" "crowdsec-bouncer" "api-key")
                                         #:file sops-sublation-secrets-file))))
           (network "crowdsec-network")
           (ports
            '("127.0.0.1:7200:7200"         ; LAPI
              "127.0.0.1:6060:6060"))       ; Prometheus metrics
           (volumes
            (list (cons "/home/krisbalintona/services/crowdsec/data/" "/var/lib/crowdsec/data/")
                  (cons "/home/krisbalintona/services/crowdsec/config/" "/etc/crowdsec")
                  (cons (string-append (dirname (current-filename)) "/files/crowdsec/config.yaml")
                        "/etc/crowdsec/config.yaml")
                  (cons (string-append (dirname (current-filename)) "/files/crowdsec/acquis.yaml")
                        "/etc/crowdsec/acquis.yaml")
                  ;; All Caddy logs
                  (cons "/home/krisbalintona/services/caddy/log" "/var/log/caddy")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-pocket-id-socket
        home-shepherd-service-type
      (list
       (shepherd-service
         (provision '(home-oci-pocket-id-socket))
         (one-shot? #t)
         (start
          #~(lambda ()
              (mkdir-p #$pocket-id-socket-dir)
              (format #t "Socket directory exists at: ~a~%" #$pocket-id-socket-dir)
              #t))
         (documentation "Create parent directory for Copyparty socket."))))
    (simple-service 'home-oci-pocket-id
        home-oci-service-type
      (oci-extension
       (networks
        (list
         (oci-network-configuration
          (name "pocket-id-network")
          (internal? #t))))
       (containers
        (list
         (let ((encryption-key-file
                (get-sops-secret-path "pocket-id-encryption-key")))
           (oci-container-configuration
             (provision "pocket-id")
             (requirement '(home-oci-pocket-id-socket))
             ;; TODO 2026-01-02: Use the hardened distroless images they
             ;; have.  Though requires more setup since the Pocket ID
             ;; process runs as non-root and their default distro
             ;; containers configure persmissiona automatically; to make
             ;; permissions work in a fresh container (which Guix does on
             ;; shepherd service restart) I have to set up those
             ;; permissions myself.  See https://pocket-id.org/d
             ;; ocs/advanced/hardening
             (image "ghcr.io/pocket-id/pocket-id:v2")
             (environment
              (list "PORT=3111"
                    "TRUST_PROXY=true"    ; Whether behind a reverse proxy
                    "APP_URL=https://pocket-id.kristofferbalintona.me"
                    (cons "ENCRYPTION_KEY_FILE" encryption-key-file)
                    "PUID=1000"             ; Default
                    "PGID=1000"             ; Default
                    ;; Use unix sockets (UDS)
                    (cons "UNIX_SOCKET" pocket-id-socket)
                    ;; When false (default), send a "heartbeat" to add my
                    ;; instance to the total Pocket ID count.  Although
                    ;; I'd like to keep this false, this container's
                    ;; network is internal, so it cannot actually send the
                    ;; heartbeat.  This results in a bunch of extraneous
                    ;; messages in the log.  Maybe in the future I can
                    ;; figure out how to get around this while maintaining
                    ;; network security...
                    "ANALYTICS_DISABLED=true"))
             (network "pocket-id-network")
             (volumes
              `(("/home/krisbalintona/services/pocket-id/data" . "/app/data")
                ,(cons encryption-key-file encryption-key-file)
                ,(cons pocket-id-socket-dir pocket-id-socket-dir)))
             (extra-arguments '("--userns=keep-id"))
             (auto-start? #t)
             (respawn? #f)))))))
    (simple-service 'home-oci-pihole
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (let* ((password-file
                 (get-sops-secret-path "pihole-webserver-password")))
           (oci-container-configuration
             (provision "pihole")
             (image "docker.io/pihole/pihole:latest")
             (environment
              `("TZ=America/Chicago"
                "FTLCONF_dns_port=53" ; Default port
                ;; Webserver
                "FTLCONF_webserver_port=127.0.0.1:7080"
                ,(cons "WEBPASSWORD_FILE" password-file)
                ;; Listen for queries on all interfaces and from all
                ;; origins
                "FTLCONF_dns_listeningMode=ALL"
                ;; Forward queries to Unbound DNS (whose port is 5335 on
                ;; the host)
                "FTLCONF_dns_upstreams=127.0.0.1#5335"
                ;; Pihole as NTP server for other devices?
                "FTLCONF_ntp_ipv4_active=false"
                "FTLCONF_ntp_ipv6_active=false"
                ;; Pihole as NTP server for host?
                "FTLCONF_ntp_sync_active=false"))
             ;; We use the host network since clients need to directly
             ;; talk to pihole otherwise pihole can't distinguish clients
             ;; (an internal container network would make all clients come
             ;; from the same client)
             (network "host")
             (volumes
              (list (cons "/home/krisbalintona/services/pihole/data" "/etc/pihole")
                    (cons password-file password-file)))
             (auto-start? #t)
             (respawn? #f)))))))
    (simple-service 'home-oci-caddy
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (let ((netlify-access-token
                (get-sops-secret-path "caddy/netlify-access-token"))
               (pocket-id-client-id
                (get-sops-secret-path "caddy/pocket-id/client-id"))
               (pocket-id-client-secret
                (get-sops-secret-path "caddy/pocket-id/client-secret"))
               (crowdsec-bouncer-api-key
                (get-sops-secret-path "caddy/crowdsec-bouncer/api-key")))
           (oci-container-configuration
             (provision "caddy")
             (requirement '(home-oci-copyparty-socket home-oci-pocket-id-socket))
             (image
               (oci-image
                 ;; OCI images locations follow a
                 ;; [registry]/[repository]:[tag] format.  Since we are
                 ;; local, we only need to specify a repository and
                 ;; optionally a tag.  The REPOSITORY field below
                 ;; corresponds to the [repository] of an OCI image
                 ;; location; it can be whatever we want since this image is
                 ;; created locally (in the Guix store)
                 (repository "caddy-security-netlify-crowdsec-coraza-maxmind")
                 (tag "2.10.2")
                 (value (specifications->manifest '("coreutils"
                                                    "caddy-security-netlify-crowdsec-coraza-maxmind")))
                 (pack-options '(#:symlinks (("/bin" -> "bin")
                                             ;; MaxMind database files
                                             ("/var/lib/geoip" -> "/var/lib/geoip"))))))
             ;; These environment variables are set in the Docker image
             ;; Caddy distributes (shown by e.g. "podman image inspect
             ;; docker.io/caddy:2.10.2").  My tests show that they need to
             ;; be set for some reason
             (environment
              `("CADDY_VERSION=v2.10.2"
                "XDG_CONFIG_HOME=/config"
                "XDG_DATA_HOME=/data"))
             ;; Use the host network, then for services expose to the host
             ;; only the required ports and have Caddy direct traffic to
             ;; those ports.  An added benefit to using the host network is
             ;; that it permits Caddy to log the real IP of clients, since
             ;; the NAT of the Podman bridge network is no longer an
             ;; intermediary
             (network "host")
             (volumes
              (list (cons "/home/krisbalintona/services/caddy/data" "/data") ; Path of XDG_DATA_HOME
                    (cons "/home/krisbalintona/services/caddy/log" "/data/log")
                    (cons (string-append (dirname (current-filename)) "/files/caddy/Caddyfile")
                          "/config/Caddyfile")
                    ;; Goaccess real-time web page
                    (cons "goaccess_web" "/var/www/goaccess")
                    ;; Unix sockets
                    (cons copyparty-socket-dir copyparty-socket-dir)
                    (cons pocket-id-socket-dir pocket-id-socket-dir)
                    ;; Secrets.  Reference these in the Caddyfile with
                    ;; file placeholders; see
                    ;; https://caddyserver.com/docs/conventions#placeholders
                    (cons netlify-access-token netlify-access-token)
                    (cons pocket-id-client-id pocket-id-client-id)
                    (cons pocket-id-client-secret pocket-id-client-secret)
                    (cons crowdsec-bouncer-api-key crowdsec-bouncer-api-key)))
             (command '("caddy" "run" "--config" "/config/Caddyfile"))
             (auto-start? #t)
             (respawn? #f)))))))
    (simple-service 'home-oci-copyparty-socket
        home-shepherd-service-type
      (list
       (shepherd-service
         (provision '(home-oci-copyparty-socket))
         (one-shot? #t)
         (start
          #~(lambda ()
              (mkdir-p #$copyparty-socket-dir)
              (format #t "Socket directory exists at: ~a~%" #$copyparty-socket-dir)
              #t))
         (documentation "Create parent directory for Copyparty socket."))))
    (simple-service 'home-oci-copyparty
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "copyparty")
           (requirement '(home-oci-copyparty-socket))
           (image "docker.io/copyparty/ac:1.19.21")
           ;; Have files mounted at /data and copyparty config + cache
           ;; files in /srv
           (volumes
            `(("/home/krisbalintona/services/copyparty/data" . "/data")
              ("/home/krisbalintona/services/copyparty/log" . "/var/log/copyparty")
              (,(string-append (dirname (current-filename)) "/files/copyparty/copyparty.conf")
               . "/srv/copyparty.conf")
              ,(cons copyparty-socket-dir copyparty-socket-dir)))
           (command '("-c" "/srv/copyparty.conf"
                      "--chdir" "/srv"
                      ;; Logging
                      "-lo" "/var/log/copyparty/copyparty-%Y-%m%d-%H%M%S.txt"))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-vaultwarden
        home-oci-service-type
      (oci-extension
       (networks
        (list
         (oci-network-configuration
          (name "vaultwarden-network"))))
       (containers
        (list
         (oci-container-configuration
           (provision "vaultwarden")
           (image "vaultwarden/server:latest")
           (environment
            `("DOMAIN=https://vault.home.kristofferbalintona.me"
              ,(cons "ADMIN_TOKEN"
                     "$argon2id$v=19$m=19456,t=2,p=1$gbzi7DRoZ+SnGVSkAuZ482w7fkXTHrdRcHUJMG24CfI$vs2Xu3ikIopqOJYf319nGEtyz08NBuXE4I9gWVRjUew")
              ;; TODO 2025-12-25: Add logrotate configuration?
              ;; Logging
              "LOG_FILE=/var/log/vaultwarden/vaultwarden.log"
              "LOG_LEVEL=debug"
              "EXTENDED_LOGGING=true"
              ;; Push notification support.  Also requires (i) the
              ;; "firebaseinstallations.googleapis.com" domain not to be
              ;; blocked and (ii) be able to make outbound HTTPS
              ;; connections (e.g., requires an "external" container
              ;; network).  See
              ;; https://github.com/dani-garcia/vaultwarden/wiki/Enabling-Mobile-Client-push-notification
              "PUSH_ENABLED=true"
              ,(cons "PUSH_INSTALLATION_ID"
                     (get-sops-secret '("vaultwarden" "push-installation-id")
                                      #:file sops-sublation-secrets-file))
              ,(cons "PUSH_INSTALLATION_KEY"
                     (get-sops-secret '("vaultwarden" "push-installation-key")
                                      #:file sops-sublation-secrets-file))
              ;; Settings relevant for security.  See also
              ;; https://github.com/dani-garcia/vaultwarden/wiki/Hardening-Guide
              "SHOW_PASSWORD_HINT=false"
              "SIGNUPS_ALLOWED=false"))
           (network "vaultwarden-network")
           (ports '("127.0.0.1:7000:80"))
           (volumes
            '(("/home/krisbalintona/services/vaultwarden/data" . "/data")
              ("/home/krisbalintona/services/vaultwarden/log" . "/var/log/vaultwarden")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-gluetun
        home-oci-service-type
      (oci-extension
       (networks
        (list
         (oci-network-configuration
          (name "gluetun-network")
          (subnet "10.89.6.0/24"))))
       (containers
        (list
         (let ((wireguard_private_key
                (get-sops-secret '("gluetun" "wireguard_private_key")
                                 #:file sops-sublation-secrets-file))
               (wireguard_preshared_key
                (get-sops-secret '("gluetun" "wireguard_preshared_key")
                                 #:file sops-sublation-secrets-file)))
           (oci-container-configuration
             (provision "gluetun")
             ;; See
             ;; https://github.com/qdm12/gluetun-wiki/blob/main/setup/readme.md#setup
             ;; for instructions on setting up Gluetun
             (image "qmcgaw/gluetun:latest")
             (host-environment
              (list
               (cons "WIREGUARD_PRIVATE_KEY" wireguard_private_key)
               (cons "WIREGUARD_PRESHARED_KEY" wireguard_preshared_key)))
             (environment
              '("TZ=America/Chicago"
                ;; See
                ;; https://github.com/qdm12/gluetun-wiki/blob/main/setup/providers/windscribe.md
                ;; for Windscribe-specific Gluetun instructions
                "VPN_SERVICE_PROVIDER=windscribe"
                
                ;; Wireguard configuration
                ;;
                ;; Output from generated Wireguard config from
                ;; https://windscribe.com/myaccount#configgenerator-wireguard.
                ;; See all Wireguard options here:
                ;; https://github.com/qdm12/gluetun-wiki/blob/main/setup/options/wireguard.md
                ;;
                ;; Interface
                "VPN_TYPE=wireguard"
                "WIREGUARD_PRIVATE_KEY"
                "WIREGUARD_ADDRESSES='100.82.80.101/32'"
                ;; Peer (VPN server)
                "WIREGUARD_PUBLIC_KEY=6O1bKP+apj/JT/aV++aODc1+EHlPO+c0xGyfKXE+k14=" ; Optional
                "WIREGUARD_ENDPOINT_PORT=65142"
                "WIREGUARD_PRESHARED_KEY"
                
                ;; See
                ;; https://github.com/qdm12/gluetun-wiki/blob/main/setup/servers.md
                ;; for a list of VPN provider server regions and cities.
                ;; Choose values corresponding to the location chosen in the
                ;; config generated by Windscribe (see above)
                "SERVER_REGIONS='US East'"
                "SERVER_CITIES=Chicago"
                "UPDATER_PERIOD=24h"    ; Automatically update server list
                
                ;; Container firewall rules
                ;;
                ;; General firewall information and default behavior:
                ;; https://github.com/qdm12/gluetun-wiki/blob/main/faq/firewall.md.
                ;; See
                ;; https://github.com/qdm12/gluetun-wiki/blob/main/setup/options/firewall.md
                ;; for all firewall options
                "FIREWALL_INPUT_PORTS=9091"   ; Transmission web UI
                "FIREWALL_INPUT_PORTS=6701")) ; qBittorrent web UI
             (network "gluetun-network")
             (ports '("127.0.0.1:9091:9091"   ; Transmission web UI
                      "127.0.0.1:6701:6701")) ; qBittorrent web UI
             (extra-arguments '("--device=/dev/net/tun:/dev/net/tun"
                                "--cap-add=NET_ADMIN"
                                "--cap-add=NET_RAW")) ; For UDP health checks
             (volumes
              `(("/home/krisbalintona/services/gluetun/data" . "/gluetun")))
             (auto-start? #t)
             (respawn? #f)))))))
    (simple-service 'home-oci-transmission
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "transmission")
           (requirement '(gluetun))
           ;; See https://github.com/linuxserver/docker-transmission for
           ;; instructions on setting up Gluetun
           (image "lscr.io/linuxserver/transmission:latest")
           (environment
            '("TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"))
           ;; NOTE: The transmission web UI is exposed by the Gluetun
           ;; container
           (network "container:gluetun")
           (volumes
            '(("/home/krisbalintona/services/transmission/config" . "/config")
              ("/home/krisbalintona/services/transmission/watch" . "/watch")
              ("/home/krisbalintona/services/media/downloads/bittorrent" . "/downloads")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-qbittorrent
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "qbittorrent")
           (requirement '(gluetun))
           (image "linuxserver/qbittorrent:latest")
           (environment
            '("TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"
              "TORRENTING_PORT=6299" ; 2026-01-09: Setting this doesn't have an effect in my setup
              "WEBUI_PORT=6701"))
           (network "container:gluetun")
           (volumes
            '(("/home/krisbalintona/services/qbittorrent/config" . "/config")
              ("/home/krisbalintona/services/qbittorrent/log" . "/log")
              ("/home/krisbalintona/services/media" . "/data")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-sabnzbd
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "sabnzbd")
           (image "linuxserver/sabnzbd:latest")
           (environment
            '("TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"))
           (network "gluetun-network")
           (ports '("127.0.0.1:5790:8080"))
           (volumes
            '(("/home/krisbalintona/services/sabnzbd/data" . "/config")
              ("/home/krisbalintona/services/sabnzbd/log" . "/config/logs")
              ("/home/krisbalintona/services/media" . "/data")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-sonarr
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "sonarr")
           (image "linuxserver/sonarr:latest")
           ;; See all environment variables here:
           ;; https://wiki.servarr.com/sonarr/environment-variables
           (environment
            '("TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"
              "SONARR__SERVER__PORT=15151"))
           (network "gluetun-network")
           (ports '("127.0.0.1:15151:15151"))
           (volumes
            '(("/home/krisbalintona/services/sonarr/data" . "/config")
              ("/home/krisbalintona/services/sonarr/log" . "/config/logs")
              ("/home/krisbalintona/services/media" . "/data")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-radarr
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "radarr")
           (image "linuxserver/radarr:latest")
           ;; See all environment variables here:
           ;; https://wiki.servarr.com/radarr/environment-variables
           (environment
            '("TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"
              "RADARR__SERVER__PORT=14100"))
           (network "gluetun-network")
           (ports '("127.0.0.1:14100:14100"))
           (volumes
            '(("/home/krisbalintona/services/radarr/data" . "/config")
              ("/home/krisbalintona/services/radarr/log" . "/config/logs")
              ("/home/krisbalintona/services/media" . "/data")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-bazarr
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "bazarr")
           (image "linuxserver/bazarr:latest")
           (environment
            '("TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"))
           (network "gluetun-network")
           (ports '("127.0.0.1:9799:6767"))
           (volumes
            '(("/home/krisbalintona/services/jellyfin/data" . "/config")
              ("/home/krisbalintona/services/media" . "/data")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-profilarr
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "profilarr")
           (image "santiagosayshey/profilarr:beta")
           (environment
            '("TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"
              "UMASK=022"))
           (network "gluetun-network")
           (ports '("127.0.0.1:11200:6868"))
           (volumes
            '(("/home/krisbalintona/services/profilarr/data" . "/config")
              ("/home/krisbalintona/services/profilarr/log" . "/config/log")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-prowlarr
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "prowlarr")
           (image "lscr.io/linuxserver/prowlarr:latest")
           ;; See all environment variables here:
           ;; https://wiki.servarr.com/prowlarr/environment-variables
           (environment
            '("TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"
              "PROWLARR__SERVER__PORT=13031"))
           (network "gluetun-network")
           (ports '("127.0.0.1:13031:13031"))
           (volumes
            '(("/home/krisbalintona/services/prowlarr/data" . "/config")
              ("/home/krisbalintona/services/prowlarr/log" . "/config/logs")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-byparr
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "byparr")
           (image "thephaseless/byparr:latest")
           ;; See
           ;; https://deepwiki.com/ThePhaseless/Byparr/4.3-environment-configuration
           ;; for a list of all environment variables
           (environment '())
           (network "gluetun-network")
           (ports '("127.0.0.1:8191:8191"))
           ;; 2026-01-12: Persist the Python .venv because the GeoIP
           ;; database is downloaded on first API call, and it'd be best
           ;; not to redownload it upon every restart of the service
           (volumes '(("/home/krisbalintona/services/byparr/venv" . "/app/.venv")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-huntarr
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "huntarr")
           (image "huntarr/huntarr:latest")
           (environment '("TZ=America/Chicago"))
           (network "gluetun-network")
           (ports '("127.0.0.1:9705:9705"))
           (volumes
            '(("/home/krisbalintona/services/huntarr/data" . "/config")
              ("/home/krisbalintona/services/huntarr/log" . "/config/logs")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-cleanuparr
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "cleanuparr")
           (image "cleanuparr/cleanuparr:latest")
           (environment
            '("TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"
              "UMASK=022"
              "PORT=10001"
              "BASE_PATH="))
           (network "gluetun-network")
           (ports '("127.0.0.1:10001:10001"))
           (volumes
            '(("/home/krisbalintona/services/cleanuparr/data" . "/config")
              ("/home/krisbalintona/services/cleanuparr/log" . "/config/logs")
              ("/home/krisbalintona/services/media" . "/data")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-renamarr
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "renamarr")
           (image "ghcr.io/hollanbm/renamarr:latest")
           (environment '("TZ=America/Chicago"))
           (network "gluetun-network")
           (volumes '(("/home/krisbalintona/services/renamarr/config" . "/config")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-seerr
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "seerr")
           (image "seerr/seerr:develop")
           (environment
            '("TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"
              "PORT=5055"))
           (network "gluetun-network")
           (ports '("127.0.0.1:5055:5055"))
           (volumes
            '(("/home/krisbalintona/services/seerr/data" . "/app/config")
              ("/home/krisbalintona/services/seerr/log" . "/app/config/logs")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-jellyfin
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "jellyfin")
           ;; NOTE 2026-01-10: I've implemented the hardware acceleration
           ;; instructions specific to my device as instructed by this
           ;; maintainer; if I change the image in the future, I must
           ;; change my configuration elsewhere to support hardware
           ;; acceleration.  See those instructions here:
           ;; https://github.com/linuxserver/docker-jellyfin?tab=readme-ov-file#hardware-acceleration-enhancements
           (image "linuxserver/jellyfin:latest")
           (environment
            '("TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"
              "JELLYFIN_PublishedServerUrl=https://jellyfin.kristofferbalintona.me"
              ;; For hardware acceleration support, enable the linuxserver
              ;; OpenCL-Intel mod; see
              ;; https://github.com/linuxserver/docker-mods/tree/jellyfin-opencl-intel.
              ;; This is just the prerequisite for hardware acceleration
              ;; support; see
              ;; https://github.com/linuxserver/docker-jellyfin?tab=readme-ov-file#hardware-acceleration-enhancements
              ;; for the full instructions.
              ;;
              ;; NOTE 2026-01-10: I believe that in order for the
              ;; linuxserver mods to be installed, the container must be
              ;; run at least once manually, not by Shepherd?
              "DOCKER_MODS=linuxserver/mods:jellyfin-opencl-intel"))
           (network "gluetun-network")
           (ports '("127.0.0.1:8096:8096"))
           (volumes
            '(("/home/krisbalintona/services/jellyfin/data" . "/config")
              ("/home/krisbalintona/services/jellyfin/cache" . "/cache")
              ("/home/krisbalintona/services/media" . "/media")))
           ;; Pass the appropriate GPU device to Jellyfin, as instructed
           ;; here:
           ;; https://jellyfin.org/docs/general/post-install/transcoding/hardware-acceleration/intel#configure-on-linux-host,
           ;; for the sake of hardware acceleration. The device is
           ;; specific to Intel GPUs.
           (extra-arguments '("--device=/dev/dri/renderD128:/dev/dri/renderD128:rwm"))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-shoko
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "shoko")
           (image "shokoanime/server:latest")
           (environment
            '("TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"
              "PORT=5055"))
           (network "gluetun-network")
           (ports '("127.0.0.1:8111:8111"))
           (volumes
            '(("/home/krisbalintona/services/shoko/data" . "/home/shoko/.shoko")
              ("/home/krisbalintona/services/media" . "/media")))
           ;; Additional argument set in the official documentation
           (extra-arguments '("--shm-size=256m"))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-lidarr
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "lidarr")
           ;; Use the Nightly branch which supports plugins.  (See
           ;; https://wiki.servarr.com/lidarr/plugins.) I use the
           ;; Tubifarry plugin to integrate with Slskd and download from
           ;; YouTube
           (image "lscr.io/linuxserver/lidarr:nightly")
           (environment
            '("TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"))
           (network "gluetun-network")
           (ports '("127.0.0.1:8686:8686"))
           (volumes
            '(("/home/krisbalintona/services/lidarr/data" . "/config")
              ("/home/krisbalintona/services/media". "/media")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-yubal
        home-oci-service-type
      (oci-extension
       (networks
        (list
         (oci-network-configuration
          (name "yubal-network"))))
       (containers
        (list
         (oci-container-configuration
           (provision "yubal")
           (image "ghcr.io/guillevc/yubal:latest")
           (container-user "1000:1000")
           (environment '("YUBAL_TZ=America/Chicago"))
           (network "yubal-network")
           (ports '("127.0.0.1:14130:8000"))
           (volumes
            '(("/home/krisbalintona/services/yubal/data" . "/app/data")
              ("/home/krisbalintona/services/yubal/config" . "/app/config")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-navidrome
        home-oci-service-type
      (oci-extension
       (networks
        (list
         (oci-network-configuration
          (name "navidrome-network"))))
       (containers
        (list
         (oci-container-configuration
           (provision "navidrome")
           (image "deluan/navidrome:latest")
           (container-user "1000:1000")
           (environment '("ND_LOGLEVEL=info"))
           (network "navidrome-network")
           (ports '("127.0.0.1:4533:4533"))
           (volumes
            '(("/home/krisbalintona/services/media/music" . "/music")
              ("/home/krisbalintona/services/navidrome/data" . "/data")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-yamtrack
        home-oci-service-type
      (oci-extension
       (networks
        (list
         (oci-network-configuration
          (name "yamtrack-network"))))
       (containers
        (list
         (oci-container-configuration
           (provision "yamtrack")
           (image "ghcr.io/fuzzygrim/yamtrack")
           (host-environment
            (list
             (cons "SECRET"
                   (get-sops-secret '("yamtrack" "django" "secret-key")
                                    #:file sops-sublation-secrets-file))
             (cons "SOCIALACCOUNT_PROVIDERS"
                   (format #f
                           "{\"openid_connect\": {
               \"OAUTH_PKCE_ENABLED\": true,
               \"APPS\": [{
                 \"provider_id\": \"PocketID\",
                 \"name\": \"Pocket ID\",
                 \"client_id\": \"~a\",
                 \"secret\": \"~a\",
                 \"settings\": {
                   \"server_url\": \"https://pocket-id.kristofferbalintona.me/.well-known/openid-configuration\"
                 }
               }]
             }}"
                           (get-sops-secret '("yamtrack" "pocket-id" "client-id")
                                            #:file sops-sublation-secrets-file)
                           (get-sops-secret '("yamtrack" "pocket-id" "secret")
                                            #:file sops-sublation-secrets-file)))
             (cons "ANILIST_ID"
                   (get-sops-secret '("yamtrack" "anilist" "api-id")
                                    #:file sops-sublation-secrets-file))
             
             (cons "ANILIST_SECRET"
                   (get-sops-secret '("yamtrack" "anilist" "api-secret")
                                    #:file sops-sublation-secrets-file))
             
             (cons "STEAM_API_KEY"
                   (get-sops-secret '("yamtrack" "steam-api-key")
                                    #:file sops-sublation-secrets-file))))
           ;; A list of all environment variables:
           ;; https://github.com/FuzzyGrim/Yamtrack/wiki/Environment-Variables
           (environment
            `(;;; User and System Configuration
              "TZ=America/Chicago"
              "PUID=1000"
              "PGID=1000"
              "ACCOUNT_DEFAULT_HTTP_PROTOCOL=https"
              ;; See also ACCOUNT_LOGOUT_REDIRECT_URL
              "SOCIAL_PROVIDERS=allauth.socialaccount.providers.openid_connect"
              "SOCIALACCOUNT_PROVIDERS"
              "SOCIALACCOUNT_ONLY=true"
              
              ;;; Redis and Django Settings
              "REDIS_URL=redis://yamtrack-redis:6379" ; Default redis port
              "REDIS_PREFIX=yamtrack"
              "URLS=https://yamtrack.kristofferbalintona.me"
              "SECRET"
              "REGISTRATION=true"
              "ADMIN_ENABLED=true"
    
              ;;; Media sources
              "TMDB_LANG=en-US"
    
              ;;; Media imports
              "STEAM_API_KEY"
              ;; I have a public Anilist account but set up an API client
              ;; in case in the future I decide to make it private
              "ANILIST_ID"
              "ANILIST_SECRET"))
           (network "yamtrack-network")
           (ports '("127.0.0.1:7878:8000"))
           (volumes '(("/home/krisbalintona/services/yamtrack/data" . "/yamtrack/db")))
           (auto-start? #t)
           (respawn? #f))
         (oci-container-configuration
           (provision "yamtrack-redis")
           (image "docker.io/library/redis:8-alpine")
           (network "yamtrack-network")
           (volumes '(("/home/krisbalintona/services/yamtrack/redis-data" . "/data")))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-immich
        home-oci-service-type
      (let* ((immich-network-subnet "10.89.12.0/24")
             (immich-version "v2.4.1")
             ;; These Database (postgres) variables are the ones that I've
             ;; customized; they must be provided (as env vars inside the
             ;; container) to all Immich workers (i.e., the server and
             ;; machine learning containers).  See
             ;; https://docs.immich.app/install/environment-variables/#database
             (db-hostname "DB_HOSTNAME=immich-postgres")
             (db-password "DB_PASSWORD=s6nshtP8kkKcjF")
             (db-storage-type "DB_STORAGE_TYPE=HDD") ; I use an HDD
             ;; Do not change these two variables
             (db-username "DB_USERNAME=postgres")
             (db-database-name "DB_DATABASE_NAME=immich")
             (db-env-vars
              (list db-hostname db-password db-storage-type db-username db-database-name))
             ;; These Redis variables are the ones that I've customized;
             ;; they must be provided (as env vars inside the container)
             ;; to all Immich workers (i.e., the server and machine
             ;; learning containers).  See
             ;; https://docs.immich.app/install/environment-variables/#redis
             (redis-hostname "REDIS_HOSTNAME=immich-redis")
             (redis-port "REDIS_PORT=6379")  ; Default
             (redis-env-vars
              (list redis-hostname redis-port)))
        (oci-extension
         (networks
          (list
           (oci-network-configuration
            (name "immich-network")
            (subnet immich-network-subnet))))
         ;; All supported environment variables can be found:
         ;; https://docs.immich.app/install/environment-variables
         (containers
          (list
           ;; Worker containers
           (oci-container-configuration
             (provision "immich-server")
             (image (string-append "ghcr.io/immich-app/immich-server:" immich-version))
             (requirement '(immich-redis immich-postgres))
             ;; (host-environment (append db-env-vars redis-env-vars))
             (environment
              (cons*
               "TZ=America/Chicago"
               "IMMICH_PORT=2283"          ; Default
               (cons "IMMICH_TRUSTED_PROXIES"
                     immich-network-subnet) ; Trust the Caddy reverse proxy
               (append db-env-vars
                       redis-env-vars)))
             ;; Enable hardware transcoding (Intel QuickSync).  This is
             ;; specific to my hardware; if my hardware changes, I may
             ;; need to change the relevant settings too.  See
             ;; https://docs.immich.app/features/hardware-transcoding and
             ;; the linked hwaccel.transcoding.yml file
             (extra-arguments '("--device=/dev/dri/renderD128:/dev/dri/renderD128:rwm"))
             (network "immich-network")
             (ports '("127.0.0.1:2283:2283"))
             (volumes
              '("/etc/localtime:/etc/localtime:ro"
                ("/home/krisbalintona/services/immich/data" . "/data")))
             (auto-start? #t)
             (respawn? #f))
           (oci-container-configuration
             (provision "immich-machine-learning")
             (image (string-append "ghcr.io/immich-app/immich-machine-learning:" immich-version))
             ;; NOTE 2026-01-21: My machine does not support
             ;; "Hardware-Accelerated Machine Learning," so I have not
             ;; inserted the relevant configuration from their
             ;; hwaccel.ml.yml file.  But in the future that may change.
             ;; See
             ;; https://docs.immich.app/features/ml-hardware-acceleration/.
             ;; This also involves changing the container image used,
             ;; e.g., adding the "-cuda" suffix
             ;; (host-environment (append db-env-vars redis-env-vars))
             (environment
              (cons*
               "TZ=America/Chicago"
               (append db-env-vars
                       redis-env-vars)))
             (network "immich-network")
             (volumes '(("/home/krisbalintona/services/yamtrack/data" . "/yamtrack/db")))
             (auto-start? #t)
             (respawn? #f))
    
           ;; Helper containers
           (oci-container-configuration
             (provision "immich-redis")
             (image "docker.io/valkey/valkey:9@sha256:fb8d272e529ea567b9bf1302245796f21a2672b8368ca3fcb938ac334e613c8f")
             (network "immich-network")
             (environment
              (cons*
               "TZ=America/Chicago"
               redis-env-vars))
             (volumes '(("/home/krisbalintona/services/immich/redis-data" . "/data")))
             (auto-start? #t)
             (respawn? #f))
           (oci-container-configuration
             ;; Refereed to as the "database" in the documentation
             (provision "immich-postgres")
             (image "ghcr.io/immich-app/postgres:14-vectorchord0.4.3-pgvectors0.2.0@sha256:bcf63357191b76a916ae5eb93464d65c07511da41e3bf7a8416db519b40b1c23")
             (environment
              (list
               "TZ=America/Chicago"
               (cons "POSTGRES_PASSWORD" db-password)
               (cons "POSTGRES_USER" db-username)
               (cons "POSTGRES_DB" db-database-name)
               (cons "POSTGRES_INITDB_ARGS" "--data-checksums")
               "DB_STORAGE_TYPE"))
             (extra-arguments '("--shm-size=256m"))
             (network "immich-network")
             (volumes
              '(("/home/krisbalintona/services/immich/postgres-data" . "/var/lib/postgresql/data")))
             (auto-start? #t)
             (respawn? #f)))))))
    (simple-service 'home-oci-goaccess
        home-oci-service-type
      (oci-extension
       (networks
        (list
         (oci-network-configuration
          (name "goaccess-network")
          (internal? #t))))
       (containers
        (list
         (oci-container-configuration
           (provision "goaccess")
           (image
             (oci-image
               (repository "goaccess")
               (tag "1.9.3")
               (value (specifications->manifest '("coreutils"
                                                  "goaccess")))
               (pack-options '(#:symlinks (("/bin" -> "bin"))))))
           (network "goaccess-network")
           (ports '("127.0.0.1:7890:7890"))
           (volumes
            `(("/home/krisbalintona/services/caddy/log" . "/var/log/caddy")
              ("goaccess_web" . "/var/www/goaccess")))
           ;; Command taken from here:
           ;; https://dev.to/emrancu/setup-goaccess-in-ubuntulinux-with-docker-and-real-cad-access-over-domainsub-domain-226n
           ;;
           ;; Goaccess uses websockets for real-time updates.  We can
           ;; confirm that the goaccess page we see is receiving real-time
           ;; updates from the green dot on the top left of the page
           ;; (beside the burger menu icon).
           ;;
           ;; Caddy directs requests to our nselected domain to the HTTPS
           ;; port (443) of goaccess's network (i.e., the container
           ;; network).  As such, Caddy expects the goaccess's websocket
           ;; to be at wss://DOMAIN:443/ws.
           ;; 
           ;; (And we don't have to worry about passing SSL information to
           ;; the goaccess invocation certificates with the "tls internal"
           ;; Caddy setting.)
           ;;
           ;; Caddy just listens to the websocket to know when to update
           ;; the files it serves, but the actual file it serves is at a
           ;; path accessible in its container.  Caddy then knows to just
           ;; serve these files via the "file_server" setting.
           (command '("goaccess"
                      "/var/log/caddy/copyparty-json.log"
                      "/var/log/caddy/vaultwarden-json.log"
                      "--log-format=CADDY"
                      "-o" "/var/www/goaccess/index.html"
                      "--real-time-html"
                      "--ws-url=wss://goaccess.home.kristofferbalintona.me:443/ws"
                      "--port=7890"
                      "--tz=America/Chicago"))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-gatus
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "gatus")
           (image "twinproduction/gatus:stable")
           (network "host")
           (volumes
            `(,(cons (string-append (dirname (current-filename)) "/files/gatus/config.yaml")
                     "/config/config.yaml")
              "/home/krisbalintona/services/gatus/data:/data"))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-grafana
        home-oci-service-type
      (oci-extension
       (networks
        (list
         (oci-network-configuration
          (name "grafana-network"))))
       (containers
        (list
         (oci-container-configuration
           (provision "grafana")
           (image "grafana/grafana:latest")
           (container-user "1000")
           (environment
            '("GF_SERVER_PROTOCOL=http" ; Keep HTTP; reverse proxy handles HTTPS
              "GF_SERVER_DOMAIN=grafana.home.kristofferbalintona.me"
              "GF_SERVER_ROOT_URL=https://grafana.home.kristofferbalintona.me/"
              "GF_SERVER_ENFORCE_DOMAIN=True"))
           (volumes '(("/home/krisbalintona/services/grafana/data" . "/var/lib/grafana")))
           (network "grafana-network")
           (ports '("127.0.0.1:3000:3000")) ; Grafana web UI
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-victoria-metrics
        home-podman-pods-service-type
      (oci-pod-extension
       (containers
        (list
         (oci-container-configuration
           (provision "victoria-metrics")
           (image "victoriametrics/victoria-metrics:latest")
           (container-user "1000")
           (volumes
            '(("/home/krisbalintona/services/victoria-metrics/data" . "/data")
              ("/home/krisbalintona/services/victoria-metrics/config" . "/config:ro")))
           (network "grafana-network")
           (ports '("127.0.0.1:8428:8428")) ; Web UI
           ;; We scrape purely via vmagent, so we shouldn't specify
           ;; -promscrape.config
           (command
            '("-storageDataPath=/data"
              "-httpListenAddr=0.0.0.0:8428" ; Default
              "-retentionPeriod=6M"))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-vmagent
        home-podman-pods-service-type
      (oci-pod-extension
       (containers
        (list
         (oci-container-configuration
           (provision "vmagent")
           (image "quay.io/victoriametrics/vmagent:latest")
           (volumes
            '(("/home/krisbalintona/services/victoria-metrics/config" . "/config:ro")))
           ;; Automatically forward all ports on host loopback to
           ;; container loopback.  See
           ;; https://passt.top/builds/latest/web/passt.1.html for
           ;; information on why pasta options create that result
           (network "pasta:--tcp-ns,auto")
           (ports '("127.0.0.1:8429:8429"))
           ;; See also the -remoteWrite.tmpDataPath option, which defaults
           ;; to "vmagent-remotewrite-data" and is responsible for acting
           ;; as a buffer for scraped data
           (command
            '("-httpListenAddr=0.0.0.0:8429" ; Default
              "-promscrape.config=/config/scrape.yaml"
              ;; Host + port + /api/v1/write of VictoriaMetrics
              "-remoteWrite.url=http://127.0.0.1:8428/api/v1/write"
              ;; In case I accidentally include lines in the scrape config
              ;; that is incompatible with vmagent; see
              ;; https://docs.victoriametrics.com/victoriametrics/vmagent/#unsupported-prometheus-config-sections
              "-promscrape.config.strictParse=false"))
           (auto-start? #t)
           (respawn? #f))))))
    (service home-restic-backup-service-type)
    (simple-service 'home-restic-vault
        home-restic-backup-service-type
      (list
       (restic-job/defaults
        #:name "restic-vault"
        #:schedule "0 7-22/3 * * *"
        #:files (list (string-append (getenv "HOME") "/vault")))))
    (simple-service 'home-restic-crowdsec
        home-restic-backup-service-type
      (list
       (restic-job/defaults
        #:name "restic-crowdsec"
        #:schedule "0 0 */2 * *"
        #:files (list (string-append services-dir "/crowdsec")))))
    (simple-service 'home-restic-pocket-id
        home-restic-backup-service-type
      (list
       (restic-job/defaults
        #:name "restic-pocket-id"
        #:schedule "0 0 */4 * *"
        #:files (list (string-append services-dir "/pocket-id/data")))))
    (simple-service 'home-restic-caddy
        home-restic-backup-service-type
      (list
       (restic-job/defaults
        #:name "restic-caddy"
        #:schedule "0 0 */2 * *"
        #:files (list (string-append services-dir "/caddy")))))
    (simple-service 'home-restic-copyparty
        home-restic-backup-service-type
      (list
       (restic-job/defaults
        #:name "restic-copyparty"
        #:schedule "0 12 * * *"
        #:files (list (string-append services-dir "/copyparty")))))
    (simple-service 'home-restic-vaultwarden
        home-restic-backup-service-type
      (list
       (restic-job/defaults
        #:name "restic-vaultwarden"
        #:schedule "0 0 * * *"
        #:files (list (string-append services-dir "/vaultwarden")))))
    
    (simple-service 'krisb-symlink-git-config-files-service-type
        home-xdg-configuration-files-service-type
      `(("git/config"
         ,(local-file "files/git/config"))))
    
    (service home-files-service-type
      `((".guile" ,%default-dotguile)
        (".Xdefaults" ,%default-xdefaults)))

    (service home-xdg-configuration-files-service-type
      `(("gdb/gdbinit" ,%default-gdbinit)
        ("nano/nanorc" ,%default-nanorc)))

    %base-home-services)))
