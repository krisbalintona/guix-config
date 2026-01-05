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
             (gnu packages terminals)
             (krisb services shells)
             (gnu packages gnupg)
             (gnu home services gnupg)
             (sops secrets)
             (sops home services sops)
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
     "git"
     "make"
     "cmake"
     "python"
     "tree"
     "ripgrep"
     "fd"
     "jq"
     "parted"
     "bat"
     "procs"
     "eza"
     "brightnessctl"
     "jujutsu"
     "fzf"
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
     "restic")))
  
  (services
   (cons*
    (simple-service 'common-environment-variables
        home-environment-variables-service-type
      '(("PAGER" . "less -RKF")))
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
    (simple-service 'home-fish-eza
        home-fish-service-type
      (home-fish-extension
        (aliases `(("ls" . "eza")
                   ("la" . "eza -la")))))
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
    (service home-atuin-service-type
      (home-atuin-configuration
        (atuin-fish-flags '("--disable-up-arrow"))
        (atuin-bash-flags '("--disable-up-arrow"))))
    (simple-service 'krisb-symlink-atuin-config-files-service-type
        home-xdg-configuration-files-service-type
      `(("atuin/config.toml"
         ,(local-file "files/atuin/config.toml"))))
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
            (key '("restic-backup-password"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))))))
    (service home-oci-service-type
      (for-home
       (oci-configuration
        (runtime 'podman)                   ; Use podman instead of docker
        (verbose? #t))))
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
           (ports '("127.0.0.1:7200:7200"))
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
                      "--tz='America/Chicago'"))
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
