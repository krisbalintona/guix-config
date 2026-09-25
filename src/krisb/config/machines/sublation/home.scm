(define-module (krisb config machines sublation home)
  #:use-module (krisb config common)
  #:use-module (krisb config machines sublation common)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu system shadow)      ; For user-group 
  #:use-module (gnu home)               ; provides home-environment
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages shells)
  #:use-module (gnu home services gnupg)
  #:use-module (sops secrets)
  #:use-module (sops home services sops)
  #:use-module (gnu services containers)
  #:use-module (gnu home services containers)
  #:use-module (gnu packages containers)
  #:use-module (gnu home services ssh)
  #:use-module (sops services sops)
  #:use-module (krisb packages networking)
  #:use-module (gnu services containers)
  #:use-module (gnu home services containers)
  #:use-module (sops services sops)
  #:use-module (gnu services containers)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services containers)
  #:use-module (sops services sops)
  #:use-module (krisb services envsubst)
  #:use-module (krisb services home envsubst)
  #:use-module (sops services sops)
  #:use-module (sops services sops)
  #:use-module (sops services sops)
  #:use-module (gnu services containers)
  #:use-module (gnu home services containers)
  #:use-module (krisb services containers)
  #:use-module (gnu home services backup)
  #:use-module (gnu home services syncthing)
  )

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
(define sops-secret-technitium-password
  (sops-secret
    (key '("technitium" "password"))
    (file (local-file sops-sublation-secrets-path))
    (permissions #o400)))
(define sops-secret-technitium-pocket-id-sso-dotenv
  (sops-secret
    (key '("technitium" "pocket-id-SSO"))
    (file (local-file sops-sublation-secrets-path))
    (output-type "dotenv")
    (permissions #o400)))
(define sops-secret-tinyauth-dotenv
  (sops-secret
    (key '("tinyauth"))
    (file (local-file sops-sublation-secrets-path))
    (output-type "dotenv")
    (permissions #o400)))
(define copyparty-socket-dir
  (string-append (getenv "XDG_RUNTIME_DIR")
                 "/copyparty"))

(define copyparty-socket
  (string-append copyparty-socket-dir "/copyparty.sock"))
(define sops-secret-copyparty-dotenv
  (sops-secret
    (key '("copyparty"))
    (file (local-file sops-sublation-secrets-path))
    (permissions #o400)
    (output-type "dotenv")))
(define sops-secret-gluetun-dotenv
  (sops-secret
    (key '("gluetun"))
    (file (local-file sops-sublation-secrets-path))
    (output-type "dotenv")
    (permissions #o400)))
(define sops-secret-qsticky-dotenv
  (sops-secret
    (key '("qsticky"))
    (file (local-file sops-sublation-secrets-path))
    (output-type "dotenv")
    (permissions #o400)))
(define sops-secret-profilarr-dotenv
  (sops-secret
    (key '("profilarr"))
    (file (local-file sops-sublation-secrets-path))
    (permissions #o400)
    (output-type "dotenv")))
(define sops-secret-navidrome-dotenv
  (sops-secret
    (key '("navidrome"))
    (file (local-file sops-sublation-secrets-path))
    (permissions #o400)
    (output-type "dotenv")))
(define sops-secret-readeck-dotenv
  (sops-secret
    (key '("readeck"))
    (file (local-file sops-sublation-secrets-path))
    (permissions #o400)
    (output-type "dotenv")))

(define-public sublation-home-environment
  (home-environment
    (packages
     (append
      common-home-packages
      (specifications->packages
       (list
        "glibc" ; 2026-09-19: For Emacs Ghostel's shell auto-detection, via 'getent'
        ;; Editors
        "vim"
        "neovim"
        "emacs"
        ;; Other packages
        "brightnessctl"
        "pinentry"
        "bind:utils"
        "soju"
        "smartmontools"            ; For smartctl
        "mergerfs"
        "mergerfs-tools"
        "btrfs-progs"
        "compsize"
        "lsof"
        ))))
    
    (services
     (append
      common-home-services
      (cons*
       (service home-fish-service-type
         (home-fish-configuration
           (config
            (list
             ;; TODO 2026-09-20: Should I open a but report in Guix about
             ;; this issue?  I need to add Guix's paths into SSH sessions
             ;; because the default only adds it to login sessions
             (plain-file "path_in_ssh.fish"
               "set -q SSH_CONNECTION; and not status is-interactive; and set -gx PATH (/bin/sh -lc 'echo $PATH' | string split :)")
             (plain-file "non_interactive_early_return.fish" "status is-interactive; or return")
             (plain-file "fish_greeting.fish" "set -g fish_greeting")))))
       (service home-gpg-agent-service-type)
       (service home-sops-secrets-service-type
         (home-sops-service-configuration
           (age-key-file %sublation-sops-age-key-file)
           (verbose? #t)
           (secrets
            (list
             (sops-secret
               (key '("pocket-id-encryption-key"))
               (file (local-file sops-sublation-secrets-path))
               (permissions #o400))
             sops-secret-technitium-password
             sops-secret-technitium-pocket-id-sso-dotenv
             (sops-secret
               (key '("caddy" "netlify-access-token"))
               (file (local-file sops-sublation-secrets-path))
               (permissions #o400))
             (sops-secret
               (key '("caddy" "crowdsec-bouncer" "api-key"))
               (file (local-file sops-sublation-secrets-path))
               (permissions #o400))
             sops-secret-tinyauth-dotenv
             sops-secret-copyparty-dotenv
             (sops-secret
               (key '("vaultwarden" "push-installation-id"))
               (file (local-file sops-sublation-secrets-path))
               (permissions #o400))
             (sops-secret
               (key '("vaultwarden" "push-installation-key"))
               (file (local-file sops-sublation-secrets-path))
               (permissions #o400))
             sops-secret-gluetun-dotenv
             sops-secret-qsticky-dotenv
             sops-secret-profilarr-dotenv
             sops-secret-navidrome-dotenv
             sops-secret-readeck-dotenv))))
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
             (openssh-host
               (name "codeberg.org")
               (user "git")
               (identity-file "~/.ssh/2026-git-forges"))))))
       (simple-service 'home-bash-keychain
           home-bash-service-type
         (home-bash-extension
           (bash-profile (list (local-file (config-files-path "keychain/sublation.bash"))))))
       (simple-service 'home-fish-keychain
           home-fish-service-type
         (home-fish-extension
           (config
            (list (local-file (config-files-path "keychain/sublation.fish"))))))
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
                     (string-append "COLLECTIONS="
                                    (string-join '("crowdsecurity/linux"
                                                   "crowdsecurity/sshd"
                                                   "crowdsecurity/whitelist-good-actors"
                                                   "crowdsecurity/base-http-scenarios"
                                                   "crowdsecurity/caddy")
                                                 " "))
                     ;; Enable Write-Ahead Logging with SQLite.  Disable if
                     ;; using a filesystem over the network, e.g., NAS
                     '("USE_WAL" . "true")
                     ;; Bouncers
                     (cons "BOUNCER_KEY_caddy"
                           (get-sops-secret '("caddy" "crowdsec-bouncer" "api-key")
                                            #:file sops-sublation-secrets-path))))
              (network "crowdsec-network")
              (ports
               '("127.0.0.1:7200:7200"         ; LAPI
                 "127.0.0.1:6060:6060"))       ; Prometheus metrics
              (volumes
               (list (cons "/home/krisbalintona/services/crowdsec/data/" "/var/lib/crowdsec/data/")
                     (cons "/home/krisbalintona/services/crowdsec/config/" "/etc/crowdsec")
                     (cons (config-files-path "/crowdsec/config.yaml")
                           "/etc/crowdsec/config.yaml")
                     (cons (config-files-path "crowdsec/acquis.yaml")
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
                (image "ghcr.io/pocket-id/pocket-id:latest")
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
       (simple-service 'home-oci-technitium
           home-oci-service-type
         (oci-extension
          (containers
           (list
            (let ((container-log-dir "/var/log/technitium/dns")
                  (web-ui-port "5380")
                  (env-file
                   (sops-secret->secret-file
                    sops-secret-technitium-pocket-id-sso-dotenv
                    #:directory (string-append "/run/user/" (number->string (getuid)) "/secrets")))
                  (password-file
                   (sops-secret->secret-file
                    sops-secret-technitium-password
                    #:directory (string-append "/run/user/" (number->string (getuid)) "/secrets"))))
              (oci-container-configuration
                (provision "technitium")
                (image "technitium/dns-server:latest")
                (requirement
                 '(home-sops-secret-technitium/password home-sops-secret-technitium/pocket-id-SSO))
                ;; See
                ;; https://github.com/TechnitiumSoftware/DnsServer/blob/master/DockerEnvironmentVariables.md
                ;; for the documentation of all environment variables, and
                ;; https://github.com/TechnitiumSoftware/DnsServer/blob/master/docker-compose.yml
                ;; for an example Docker compose file
                (environment
                 (list (cons "DNS_SERVER_LOG_FOLDER_PATH" container-log-dir)
                       (cons "DNS_SERVER_DOMAIN" "technitium.home.kristofferbalintona.me")
       
                       ;; Web UI/service.  We use Caddy as a reverse proxy
                       (cons "DNS_SERVER_WEB_SERVICE_HTTP_PORT" web-ui-port)
                       (cons "DNS_SERVER_WEB_SERVICE_LOCAL_ADDRESSES" "127.0.0.1")
                       (cons "DNS_SERVER_WEB_SERVICE_REVERSE_PROXY_ADDRESSES" "127.0.0.1")
                       (cons "DNS_SERVER_WEB_SERVICE_ENABLE_HTTPS" "false") ; Default value
                       (cons "DNS_SERVER_ADMIN_PASSWORD_FILE" password-file)
       
                       ;; Who do we allow to query the DNS server?
                       (cons "DNS_SERVER_RECURSION" "UseSpecifiedNetworkACL")
                       (cons "DNS_SERVER_RECURSION_NETWORK_ACL"
                             (string-join '("127.0.0.1"      ; This machine
                                            "192.168.1.0/24" ; LAN subnet
                                            "10.0.0.0/24")   ; Wireguard VPN
                                          ","))
       
                       ;; Regarding actual DNS resolution: although
                       ;; Technitium can handle DoH and DoT, we do so with
                       ;; Caddy for both.  This means we don't have to renew
                       ;; any SSL certificates ourselves; we let Caddy do
                       ;; it for us.
                       ;;
                       ;; I enable DNS-over-HTTP in Technitium and have Caddy
                       ;; forward requests from a chosen endpoint to the port
                       ;; Technitium is listening on.  This gives us
                       ;; DNS-over-HTTPS (DoH).
                       ;;
                       ;; For DoT, we use the caddy-l4 app to handle TLS
                       ;; termination. We enable "DNS-over-TCP-PROXY" in
                       ;; Technitium and point Caddy's caddy-l4 block at that
                       ;; port, with the PROXY protocol carrying the real
                       ;; client IP.
       
                       "DNS_SERVER_LOG_USING_LOCAL_TIME=true" ; Default value
                       "DNS_SERVER_ENABLE_BLOCKING=true")) ; Network filtering
                (extra-arguments
                 ;; All SSO-related env vars are set in the env file
                 (list "--env-file" env-file))
                (network "host")
                (volumes
                 (list (cons "/etc/localtime" "/etc/localtime:ro")
                       (cons password-file password-file)
                       (cons "/home/krisbalintona/services/technitium/config" "/etc/dns")
                       (cons "/home/krisbalintona/services/technitium/logs" container-log-dir)))
                (auto-start? #t)
                (respawn? #f)))))))
       (simple-service 'home-oci-caddy
           home-oci-service-type
         (oci-extension
          (containers
           (list
            (let ((netlify-access-token
                   (get-sops-secret-path "caddy/netlify-access-token"))
                  (crowdsec-bouncer-api-key
                   (get-sops-secret-path "caddy/crowdsec-bouncer/api-key")))
              (oci-container-configuration
                (provision "caddy")
                (requirement '(copyparty-socket home-oci-pocket-id-socket))
                (image
                  (oci-image
                    ;; OCI images locations follow a
                    ;; [registry]/[repository]:[tag] format.  Since we are
                    ;; local, we only need to specify a repository and
                    ;; optionally a tag.  The REPOSITORY field below
                    ;; corresponds to the [repository] of an OCI image
                    ;; location; it can be whatever we want since this image
                    ;; is created locally (in the Guix store)
                    (repository "caddy-netlify-crowdsec-coraza-maxmind-l4")
                    (tag "2.11.4")
                    (value (specifications->manifest '("coreutils"
                                                       "caddy-netlify-crowdsec-coraza-maxmind-l4")))
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
                       (cons (config-files-path "caddy/Caddyfile")
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
                       (cons crowdsec-bouncer-api-key crowdsec-bouncer-api-key)))
                (command '("caddy" "run" "--config" "/config/Caddyfile"))
                (auto-start? #t)
                (respawn? #f)))))))
       (simple-service 'home-oci-tinyauth
           home-oci-service-type
         (oci-extension
          (containers
           (list
            (let ((port "3070")
                  (env-file
                   (sops-secret->secret-file
                    sops-secret-tinyauth-dotenv
                    #:directory (string-append "/run/user/" (number->string (getuid)) "/secrets"))))
              (oci-container-configuration
                (provision "tinyauth")
                (requirement '(home-sops-secret-tinyauth))
                (image "ghcr.io/tinyauthapp/tinyauth:latest")
                ;; TinyAuth has documentation for its integration with Pocket
                ;; ID: https://tinyauth.app/docs/guides/pocket-id/.
                ;;
                ;; See also an example for integrating TinyAuth into Caddy:
                ;; https://tinyauth.app/docs/community/caddy/
                ;;
                ;; See https://tinyauth.app/docs/reference/configuration/ for
                ;; all configuration options
                (environment
                 (list (cons "TINYAUTH_SERVER_PORT" port)
                       (cons "TINYAUTH_APPURL" "https://tinyauth.kristofferbalintona.me")
                       (cons "TINYAUTH_LOG_LEVEL" "debug")
                       ;; Since this container has a non-bridged pasta tap,
                       ;; the host's addresses are copied into it so
                       ;; 127.0.0.1 refers to the host loopback-exposed Caddy
                       (cons "TINYAUTH_AUTH_TRUSTEDPROXIES" "127.0.0.1")
                       (cons "TINYAUTH_OAUTH_PROVIDERS_POCKETID_NAME" "Pocket ID")
                       (cons "TINYAUTH_OAUTH_AUTOREDIRECT" "pocketid") ; Auto-redirect to Pocket ID
       
                       ;; Domain configs
                       ;;
                       ;; For TinyAuth to work behind a domain, each domain
                       ;; must be registered as an "app."  These must be
                       ;; configured for every domain behind TinyAuth (in
                       ;; addition to Caddy forwarding authentication to
                       ;; TinyAuth for those domains)
                       (cons "TINYAUTH_APPS_COPYPARTY_CONFIG_DOMAIN" "party.kristofferbalintona.me")))
                (extra-arguments
                 (list
                  ;; All OIDC (Pocket ID) env vars are set in the env file
                  "--env-file" env-file
                  ;; For reaching the Pocket ID container's endpoint
                  ;; published on the host
                  "--add-host" "pocket-id.kristofferbalintona.me:host-gateway"))
                (ports (list (cons "127.0.0.1" (string-append port ":" port))))
                (volumes '(("/home/krisbalintona/services/tinyauth/data" . "/data")))
                (auto-start? #t)
                (respawn? #f)))))))
       (simple-service 'home-oci-copyparty-socket
           home-shepherd-service-type
         (list
          (shepherd-service
            (provision '(copyparty-socket))
            (one-shot? #t)
            (start
             #~(lambda ()
                 (mkdir-p #$copyparty-socket-dir)
                 (format #t "Socket directory exists at: ~a~%" #$copyparty-socket-dir)
                 #t))
            (documentation "Create parent directory for Copyparty socket."))))
       (simple-service 'home-copyparty-envsubst
           home-envsubst-service-type
         (list
          (envsubst-substitution
           (name 'copyparty)
           (template (local-file "files/copyparty/copyparty.conf.template"))
           (output "/home/krisbalintona/services/copyparty/config/copyparty.conf")
           (environment-file
            (sops-secret->secret-file
             sops-secret-copyparty-dotenv
             #:directory (string-append "/run/user/" (number->string (getuid)) "/secrets")))
           (requirement '(home-sops-secret-copyparty)))))
       (simple-service 'home-oci-copyparty
           home-oci-service-type
         (oci-extension
          (containers
           (list
            (oci-container-configuration
              (provision "copyparty")
              (requirement '(copyparty-socket home-envsubst-copyparty))
              (image "docker.io/copyparty/ac:latest")
              ;; Have served files mounted at /data and the copyparty config
              ;; + cache files in /config (~/services/copyparty/config on the
              ;; host)
              (volumes
               `(("/home/krisbalintona/services/copyparty/config" . "/config")
                 ,(cons copyparty-socket-dir copyparty-socket-dir)
                 ("/home/krisbalintona/services/copyparty/log" . "/var/log/copyparty")
                 ("/home/krisbalintona/services/copyparty/data" . "/data")
                 ("/home/krisbalintona/services/media" . "/media:ro")
                 ("/home/krisbalintona/services/jobsrv/downloads" . "/jobsrv/downloads")))
              (command '("-c" "/config/copyparty.conf"
                         "--chdir" "/config"
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
                                         #:file sops-sublation-secrets-path))
                 ,(cons "PUSH_INSTALLATION_KEY"
                        (get-sops-secret '("vaultwarden" "push-installation-key")
                                         #:file sops-sublation-secrets-path))
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
            (let ((env-file
                   (sops-secret->secret-file
                    sops-secret-gluetun-dotenv
                    #:directory (string-append "/run/user/" (number->string (getuid)) "/secrets")))
                  (http-proxy-port "16016"))
              (oci-container-configuration
                (provision "gluetun")
                ;; See
                ;; https://github.com/qdm12/gluetun-wiki/blob/main/setup/readme.md#setup
                ;; for instructions on setting up Gluetun
                (image "qmcgaw/gluetun:latest")
                (requirement '(home-sops-secret-gluetun))
                (environment
                 (list "TZ=America/Chicago"
                       ;; VPN-specific options
                       ;;
                       ;; For ProtonVPN, see
                       ;; https://github.com/qdm12/gluetun-wiki/blob/main/setup/providers/protonvpn.md
                       "VPN_SERVICE_PROVIDER=protonvpn"
       
                       ;; Wireguard configuration
                       ;;
                       ;; Output from auto-generated Wireguard config.  See
                       ;; all Wireguard options here:
                       ;; https://github.com/qdm12/gluetun-wiki/blob/main/setup/options/wireguard.md
                       ;;
                       ;; These env vars are set in the --env-file:
                       ;; - WIREGUARD_PRIVATE_KEY
                       ;; - WIREGUARD_ADDRESSES
                       ;; - WIREGUARD_PUBLIC_KEY
                       "VPN_TYPE=wireguard"
                       "VPN_PORT_FORWARDING=on"
                       "PORT_FORWARD_ONLY=on"
                       ;; See
                       ;; https://github.com/qdm12/gluetun-wiki/blob/main/setup/servers.md
                       ;; for a list of VPN provider server regions and
                       ;; cities.  Choose values corresponding to the
                       ;; location chosen in the config auto-generated (see
                       ;; above)
                       "SERVER_COUNTRIES=United States"
                       "SERVER_CITIES=Chicago"
       
                       ;; Automatically update server list.  Please see
                       ;; https://github.com/qdm12/gluetun-wiki/blob/main/setup/servers.md#update-periodically
                       ;; for the advised time periods
                       ;;
                       ;; These env vars are set in the --env-file:
                       ;; - UPDATER_PROTONVPN_EMAIL
                       ;; - UPDATER_PROTONVPN_PASSWORD
                       "UPDATER_PERIOD=672h"
                       "UPDATER_PREFER_DIRECT_DOWNLOAD=yes"
       
                       ;; Container firewall rules
                       ;;
                       ;; General firewall information and default behavior:
                       ;; https://github.com/qdm12/gluetun-wiki/blob/main/faq/firewall.md.
                       ;; See
                       ;; https://github.com/qdm12/gluetun-wiki/blob/main/setup/options/firewall.md
                       ;; for all firewall options
                       (cons "FIREWALL_INPUT_PORTS"
                             (string-join (list http-proxy-port
                                                "9091"  ; Transmission web UI
                                                "6701") ; qBittorrent web UI
                                          ","))
       
                       ;; HTTP proxy options: Gluetun can act as a tiny
                       ;; proxy, opening an HTTP port to use as a proxy for
                       ;; HTTP requests.
                       ;;
                       ;; From within the container, listen to any interface.
                       ;; So any interface that can access the container's
                       ;; HTTP proxy port can use it.  This means the host
                       ;; (because: below I publish this port to the host's
                       ;; loopback interface) and other containers in the
                       ;; gluetun network can access it.
                       "HTTPPROXY=on"
                       (cons "HTTPPROXY_LISTENING_ADDRESS"
                             (string-append ":" http-proxy-port))))
                (network "gluetun-network")
                (ports `("127.0.0.1:9091:9091" ; Transmission web UI
                         "127.0.0.1:6701:6701" ; qBittorrent web UI
                         ;; Slskd
                         "127.0.0.1:5030:5030"
                         "127.0.0.1:5031:5031"
                         "127.0.0.1:50300:50300"
                         ;; HTTP proxy (make accessible to host)
                         ,(string-append "127.0.0.1:" http-proxy-port ":" http-proxy-port)))
                (extra-arguments
                 (list "--env-file" env-file
                       "--device=/dev/net/tun:/dev/net/tun"
                       "--cap-add=NET_ADMIN"
                       "--cap-add=NET_RAW"))   ; For UDP health checks
                (volumes
                 (list (cons "/home/krisbalintona/services/gluetun/data" "/gluetun")))
                (auto-start? #t)
                (respawn? #f)))))))
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
                 "WEBUI_PORT=6701"))
              (network "container:gluetun")
              (volumes
               '(("/home/krisbalintona/services/qbittorrent/config" . "/config")
                 ("/home/krisbalintona/services/qbittorrent/log" . "/log")
                 ("/home/krisbalintona/services/media" . "/data")))
              (auto-start? #t)
              (respawn? #f))))))
       (simple-service 'home-oci-qsticky
           home-oci-service-type
         (oci-extension
          (containers
           (list
            (let ((env-file
                   (sops-secret->secret-file
                    sops-secret-qsticky-dotenv
                    #:directory (string-append "/run/user/" (number->string (getuid)) "/secrets"))))
              (oci-container-configuration
                (provision "qsticky")
                (image "ghcr.io/monstermuffin/qsticky:latest")
                (requirement '(gluetun home-sops-secret-qsticky))
                (container-user "1000:1000")
                (environment
                 '("TZ=America/Chicago"
                   "LOG_LEVEL: INFO"
       
                   ;; qBittorrent settings
                   ;;
                   ;; These env vars are set in --env-file:
                   ;; - QBITTORRENT_API_KEY
                   "QBITTORRENT_HOST=gluetun"
                   "QBITTORRENT_PORT=6701"     ; Web UI port
                   "QBITTORRENT_HTTPS=false"
       
                   ;; Gluetun settings
                   ;;
                   ;; These env vars are set in --env-file:
                   ;; - GLUETUN_AUTH_TYPE
                   ;; - GLUETUN_APIKEY
                   "GLUETUN_HOST=gluetun"))
                (extra-arguments (list "--env-file" env-file))
                (network "container:gluetun")
                (auto-start? #t)
                (respawn? #f)))))))
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
       (simple-service 'home-oci-suwayomi-server
           home-oci-service-type
         (oci-extension
          (containers
           (list
            (let ((port "4567"))
              (oci-container-configuration
                (provision "suwayomi-server")
                (requirement '(byparr))
                (image "ghcr.io/suwayomi/suwayomi-server:latest")
                (container-user "1000:1000")
                (environment
                 (list "TZ=America/Chicago"
                       (cons "BIND_PORT" port)
                       
                       "FLARESOLVERR_ENABLED=true"
                       (cons "FLARESOLVERR_URL" "http://byparr:8191/")))
                (network "gluetun-network")    ; For Byparr container
                (ports (list (string-append "127.0.0.1:" port ":" port)))
                (volumes
                 (list
                  ;; Make sure the downloads directory is mounted first,
                  ;; otherwise the other directory, which it should be nested
                  ;; inside, would shadow it
                  (cons "/home/krisbalintona/services/media/manga"
                        "/home/suwayomi/.local/share/Tachidesk/downloads")
                  (cons "/home/krisbalintona/services/suwayomi-server"
                        "/home/suwayomi/.local/share/Tachidesk")))
                (auto-start? #t)
                (respawn? #f)))))))
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
         (let ((env-file
                (sops-secret->secret-file
                 sops-secret-profilarr-dotenv
                 #:directory (string-append "/run/user/" (number->string (getuid)) "/secrets"))))
           (oci-extension
            (containers
             (list
              (oci-container-configuration
                (provision "profilarr")
                (image "ghcr.io/dictionarry-hub/profilarr:latest")
                (requirement '(home-sops-secret-navidrome))
                (environment
                 `("TZ=America/Chicago"
                   "PUID=1000"
                   "PGID=1000"
                   "UMASK=022"
                   "ORIGIN=https://profilarr.home.kristofferbalintona.me" ; Because behind reverse proxy
       
                   "AUTH=oidc"))  ; OIDC-related env vars are set in env file
                (extra-arguments
                 (list "--env-file" env-file
                       ;; For reaching the Pocket ID container's endpoint
                       ;; published on the host
                       "--add-host" "pocket-id.kristofferbalintona.me:host-gateway"))
                (network "gluetun-network")
                (ports '("127.0.0.1:11200:6868"))
                (volumes
                 '(("/home/krisbalintona/services/profilarr/data" . "/config")
                   ("/home/krisbalintona/services/profilarr/log" . "/config/log")))
                (auto-start? #t)
                (respawn? #f)))))))
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
       (simple-service 'home-oci-cleanuparr
           home-oci-service-type
         (oci-extension
          (containers
           (list
            (oci-container-configuration
              (provision "cleanuparr")
              (image "cleanuparr/cleanuparr:latest")
              (container-user "1000:1000")
              (environment
               '("TZ=America/Chicago"
                 "UMASK=022"
                 "PORT=10001"))
              (network "gluetun-network")
              (ports '("127.0.0.1:10001:10001"))
              (volumes
               '(("/home/krisbalintona/services/cleanuparr/data" . "/config")
                 ("/home/krisbalintona/services/media" . "/data")))
              (auto-start? #t)
              (respawn? #f))))))
       (simple-service 'home-oci-seerr
           home-oci-service-type
         (oci-extension
          (containers
           (list
            (oci-container-configuration
              (provision "seerr")
              (image "ghcr.io/seerr-team/seerr:latest")
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
              (extra-arguments
               (list
                ;; Pass the appropriate GPU device to Jellyfin, as instructed
                ;; here:
                ;; https://jellyfin.org/docs/general/post-install/transcoding/hardware-acceleration/intel#configure-on-linux-host,
                ;; for the sake of hardware acceleration. The device is
                ;; specific to Intel GPUs.
                "--device=/dev/dri/renderD128:/dev/dri/renderD128:rwm"
                ;; For reaching the Pocket ID container's endpoint published
                ;; on the host
                "--add-host" "pocket-id.kristofferbalintona.me:host-gateway"))
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
       (simple-service 'files-slskd
           home-files-service-type
         `(("services/slskd/data/slskd.yml"
            ,(local-file (config-files-path "slskd/slskd.yml")))
           ("services/slskd/scripts/wrtag.sh"
            ,(local-file (config-files-path "slskd/wrtag.sh")))))
       (simple-service 'home-oci-slskd
           home-oci-service-type
         (oci-extension
          (containers
           (list
            (let ((slskd-username
                   (get-sops-secret '("slskd" "web" "username")
                                    #:file sops-sublation-secrets-path))
                  (slskd-password
                   (get-sops-secret '("slskd" "web" "password")
                                    #:file sops-sublation-secrets-path))
                  (slskd-api-key
                   (get-sops-secret '("slskd" "api-key")
                                    #:file sops-sublation-secrets-path))
                  (slskd-slsk-username
                   (get-sops-secret '("slskd" "soulseek" "username")
                                    #:file sops-sublation-secrets-path))
                  (slskd-slsk-password
                   (get-sops-secret '("slskd" "soulseek" "password")
                                    #:file sops-sublation-secrets-path))
                  (wrtag-api-key
                   (get-sops-secret '("wrtag" "web-api-key")
                                    #:file sops-sublation-secrets-path)))
              (oci-container-configuration
                (provision "slskd")
                (requirement '(gluetun))
                (image "slskd/slskd:latest")
                (container-user "1000:1000")
                (host-environment
                 (list (cons "SLSKD_USERNAME" slskd-username)
                       (cons "SLSKD_PASSWORD" slskd-password)
                       (cons "SLSKD_API_KEY" slskd-api-key)
                       (cons "SLSKD_SLSK_USERNAME" slskd-slsk-username)
                       (cons "SLSKD_SLSK_PASSWORD" slskd-slsk-password)
                       (cons "WRTAG_WEB_API_KEY" wrtag-api-key)))
                ;; See
                ;; https://github.com/slskd/slskd/blob/master/docs/config.md
                ;; for a complete description of all configurable environment
                ;; variables
                (environment
                 '("APP_DIR=/app"              ; Data directory for program
                   "SLSKD_DISK_LOGGER=true"    ; Log to disk
                   "SLSKD_DOWNLOADS_DIR=/media/downloads/soulseek/complete"
                   "SLSKD_INCOMPLETE_DIR=/media/downloads/soulseek/incomplete"
                   ;; "Seeding" directory
                   "SLSKD_SHARE_CACHE_RETENTION=7200" ; Rescan every 5 days
                   "SLSKD_SHARED_DIR=[full-albums]/media/music/full-albums"
                   ;; Web UI credentials
                   "SLSKD_USERNAME"
                   "SLSKD_PASSWORD"
                   ;; Modifications remotely (from the web UI)
                   "SLSKD_REMOTE_CONFIGURATION=false"
                   "SLSKD_REMOTE_FILE_MANAGEMENT=true"
                   ;; API key for other applications
                   "SLSKD_API_KEY"
                   ;; Soulseek network credentials
                   "SLSKD_SLSK_USERNAME"
                   "SLSKD_SLSK_PASSWORD"
                   ;; For wrtag import script
                   "WRTAG_WEB_API_KEY"
                   "WRTAG_WEB_URL=wrtag:7373"))
                (network "container:gluetun")
                (ports '("127.0.0.1:8686:8686"))
                (volumes
                 '(("/home/krisbalintona/services/slskd/data" . "/app")
                   ;; TODO 2026-06-20: Ideally we don't symlink the entire
                   ;; Guix store, since this is another attack vector: it may
                   ;; expose certain things that are plain-text in the store.
                   ;;
                   ;; slskd.yml is a symlink into the store, and bind-mounts
                   ;; don't dereference symlinks.  So: mount the store itself
                   ;; so the container can actually resolve where it points.
                   "/gnu/store:/gnu/store:ro"
                   ("/home/krisbalintona/services/slskd/log" . "/logs")
                   ("/home/krisbalintona/services/media" . "/media")))
                (auto-start? #t)
                (respawn? #f)))))))
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
              (environment
               '("YUBAL_TZ=America/Chicago"
                 "YUBAL_LOG_LEVEL=debug"
                 "YUBAL_DATA=/media"
                 "YUBAL_SCHEDULER_ENABLED=false"
                 "YUBAL_SCHEDULER_CRON=0 0 */3 * *"
                 "YUBAL_FETCH_LYRICS=true"))
              (network "yubal-network")
              (ports '("127.0.0.1:14130:8000"))
              (volumes
               '(("/home/krisbalintona/services/yubal/data" . "/app/config")
                 ("/home/krisbalintona/services/media" . "/media")))
              (auto-start? #t)
              (respawn? #f))))))
       (simple-service 'home-oci-wrtag
           home-oci-service-type
         (oci-extension
          (containers
           (list
            (oci-container-configuration
              (provision "wrtag")
              (image "ghcr.io/sentriz/wrtag:v0.33.0")
              (container-user "1000:1000")
              (host-environment
               (list
                (cons "WRTAG_WEB_API_KEY"
                      (get-sops-secret '("wrtag" "web-api-key")
                                       #:file sops-sublation-secrets-path))))
              (environment
               `("WRTAG_LOG_LEVEL=debug"       ; INFO isn't very informative
                 "WRTAG_WEB_PUBLIC_URL=https://wrtag.home.kristofferbalintona.me"
                 "WRTAG_WEB_LISTEN_ADDR=:7373"
                 "WRTAG_WEB_API_KEY"
                 "WRTAG_WEB_DB_PATH=/data/wrtag.db"
                 ,(cons "WRTAG_PATH_FORMAT"
                        (string-append
                         "/media/music/full-albums/"
                         "{{ artists .Release.Artists | sort | join \"; \" | safepath }}"
                         "/({{ .Release.ReleaseGroup.FirstReleaseDate.Year }}) "
                         "{{ .Release.Title | safepath }}"
                         "{{ if not (eq .ReleaseDisambiguation \"\") }} ({{ .ReleaseDisambiguation | safepath }}){{ end }}"
                         "/{{ if gt (len .Release.Media) 1 }}d{{ pad0 2 .Media.Position }} {{ end }}{{ pad0 2 .Track.Position }}.{{ .Media.TrackCount | pad0 2 }} "
                         "{{ if .IsCompilation}}{{ artistsString .Track.Artists | safepath }} - {{ end }}"
                         "{{ .Track.Title | safepath }}{{ .Ext }}"))
                 "WRTAG_ADDON=lyrics lrclib genius musixmatch,replaygain true-peak"))
              (network "gluetun-network")
              (ports '("127.0.0.1:7373:7373"))
              (volumes
               '(;; Optional.  Used when I specify WRTAG_WEB_DB_PATH above
                 ("/home/krisbalintona/services/wrtag/data" . "/data")
                 ("/home/krisbalintona/services/media" . "/media")))
              (auto-start? #t)
              (respawn? #f))))))
       (simple-service 'home-oci-deepcrate
           home-oci-service-type
         (oci-extension
          (containers
           (list
            (oci-container-configuration
              (provision "deepcrate")
              (image "ghcr.io/jordojordo/deepcrate:latest")
              (container-user "1000:1000")
              (network "gluetun-network")
              ;; 2026-02-03: See all available environment variables here:
              ;; https://jordojordo.github.io/deepcrate/guide/configuration.html#environment-variables.
              ;; Users may also override config file values; see
              ;; https://jordojordo.github.io/deepcrate/guide/configuration.html#override-config-values-via-environment
              (environment
               '("LOG_LEVEL=debug"
                 "LOG_TO_FILE=true"
                 "LOG_DIR=/log"
                 "DEEPCRATE_DB_FILE=/config/deepcrate.sqlite"))
              (ports '("127.0.0.1:1250:8080"))
              (volumes
               '(("/home/krisbalintona/services/deepcrate/data" . "/config")
                 ("/home/krisbalintona/services/deepcrate/log" . "/log")
                 ("/home/krisbalintona/services/media/music/full-albums" . "/data")))
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
            (let ((port "4533")
                  (env-file
                   (sops-secret->secret-file
                    sops-secret-navidrome-dotenv
                    #:directory (string-append "/run/user/" (number->string (getuid)) "/secrets"))))
              (oci-container-configuration
                (provision "navidrome")
                (image "deluan/navidrome:latest")
                (container-user "1000:1000")
                (requirement '(home-sops-secret-navidrome))
                ;; All environment variables can be found here:
                ;; https://www.navidrome.org/docs/usage/configuration/options/#environment-variables
                (environment
                 (list "TZ=America/Chicago"
                       "ND_LOGLEVEL=info"
                       "ND_CONFIGFILE=/config/navidrome.toml"
                       "ND_DATAFOLDER=/data"
                       ;; This will be the main music library.  But I have to
                       ;; manually use the UI to create a second music
                       ;; library at /music/partial-albums
                       "ND_MUSICFOLDER=/music/full-albums"
                       (cons "ND_BASEURL" "https://navidrome.home.kristofferbalintona.me")
                       (cons "ND_PORT" port)
                       "ND_ENFORCENONROOTUSER=true"
       
                       "ND_SCANONSTARTUP=true"
                       "ND_SCANNER_ENABLED=true"
                       "ND_PLAYLISTSPATH=playlists/**"
                       "ND_AUTOIMPORTPLAYLISTS=true"
                       "ND_AUTOTRANSCODEDOWNLOAD=true"
       
                       "ND_BACKUP_PATH=/backups"
                       "ND_BACKUP_SCHEDULE=0 0 */2 * *"
                       "ND_BACKUP_COUNT=7"
       
                       ;; ND_LASTFM_APIKEY and ND_LASTFM_SECRET are set in
                       ;; the env file
                       "ND_LASTFM_ENABLED=true"
                       "ND_LISTENBRAINZ_ENABLED=true"))
                (extra-arguments (list "--env-file" env-file))
                (network "navidrome-network")
                (ports (list (string-append "127.0.0.1:" port ":" port)))
                (volumes
                 '(("/home/krisbalintona/services/navidrome/config" . "/config")
                   ("/home/krisbalintona/services/navidrome/data" . "/data")
                   ("/home/krisbalintona/services/media/music" . "/music:ro")))
                (auto-start? #t)
                (respawn? #f)))))))
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
                                       #:file sops-sublation-secrets-path))
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
                                               #:file sops-sublation-secrets-path)
                              (get-sops-secret '("yamtrack" "pocket-id" "secret")
                                               #:file sops-sublation-secrets-path)))
                (cons "ANILIST_ID"
                      (get-sops-secret '("yamtrack" "anilist" "api-id")
                                       #:file sops-sublation-secrets-path))
                
                (cons "ANILIST_SECRET"
                      (get-sops-secret '("yamtrack" "anilist" "api-secret")
                                       #:file sops-sublation-secrets-path))
                
                (cons "STEAM_API_KEY"
                      (get-sops-secret '("yamtrack" "steam-api-key")
                                       #:file sops-sublation-secrets-path))))
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
              (extra-arguments
               ;; For reaching the Pocket ID container's endpoint published
               ;; on the host
               (list "--add-host" "pocket-id.kristofferbalintona.me:host-gateway"))
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
                (immich-version "v2.7.5")
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
                (redis-port "REDIS_PORT=6379") ; Default
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
                (extra-arguments
                 (list
                  ;; Enable hardware transcoding (Intel QuickSync).  This is
                  ;; specific to my hardware; if my hardware changes, I may
                  ;; need to change the relevant settings too.  See
                  ;; https://docs.immich.app/features/hardware-transcoding
                  ;; and the linked hwaccel.transcoding.yml file
                  "--device=/dev/dri/renderD128:/dev/dri/renderD128:rwm"
                  ;; For reaching the Pocket ID container's endpoint
                  ;; published on the host
                  "--add-host" "pocket-id.kristofferbalintona.me:host-gateway"))
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
                (image "docker.io/valkey/valkey:9@sha256:3b55fbaa0cd93cf0d9d961f405e4dfcc70efe325e2d84da207a0a8e6d8fde4f9")
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
       (simple-service 'home-oci-stash
           home-oci-service-type
         (oci-extension
          (networks
           (list
            (oci-network-configuration
             (name "stash-network"))))
          (containers
           (list
            (let ((host-stash-dir "/home/krisbalintona/services/stash"))
              (oci-container-configuration
                (provision "stash")
                (image "stashapp/stash:latest")
                (environment
                 '(;; For reverse proxies, also need to set "external_host"
                   ;; configuration option in Stash's config.yml file.  See
                   ;; https://docs.stashapp.cc/guides/advanced-configuration-options/#external-host
                   "STASH_PORT=9999"
       
                   ;; See mounted volumes
                   "STASH_STASH=/data/"
                   "STASH_METADATA=/metadata/"
                   "STASH_CACHE=/cache/"
                   "STASH_GENERATED=/generated/"))
                (network "stash-network")
                (ports '("127.0.0.1:9999:9999"))
                (volumes
                 (list '("/etc/localtime" . "/etc/localtime:ro")
                       (cons (string-append host-stash-dir "/data") "/data")
                       (cons (string-append host-stash-dir "/generated") "/generated")
                       (cons (string-append host-stash-dir "/metadata") "/metadata")
                       (cons (string-append host-stash-dir "/cache") "/cache")
                       (cons (string-append host-stash-dir "/blobs") "/blobs")
                       (cons (string-append host-stash-dir "/config") "/root/.stash")
                       '("/home/krisbalintona/services/media/adult/downloads" . "/adult:ro")))
                (auto-start? #t)
                (respawn? #f)))))))
       (simple-service 'home-oci-readeck
           home-oci-service-type
         (oci-extension
          (networks
           (list
            (oci-network-configuration
             (name "readeck-network"))))
          (containers
           (list
            (let ((port "17800")
                  (host-data-dir "/home/krisbalintona/services/readeck")
                  (env-file
                   (sops-secret->secret-file
                    sops-secret-readeck-dotenv
                    #:directory (string-append "/run/user/" (number->string (getuid)) "/secrets"))))
              (oci-container-configuration
                (provision "readeck")
                (image "codeberg.org/readeck/readeck:latest")
                ;; FIXME 2026-08-04: Use `sops-secret->shepherd-service-name`
                ;; when/if it becomes public/exported to get the Shepherd
                ;; service name?
                (requirement '(home-sops-secret-readeck))
                (environment
                 (list (cons "READECK_SERVER_PORT" port)
                       "READECK_SERVER_BASE_URL=https://readeck.home.kristofferbalintona.me"
                       "READECK_ALLOWED_HOSTS=readeck.home.kristofferbalintona.me"))
                (extra-arguments
                 ;; Sets READECK_SECRET_KEY env var
                 (list "--env-file" env-file))
                (network "readeck-network")
                (ports (list (string-append "127.0.0.1:" port ":" port)))
                (volumes (list (cons host-data-dir "/readeck")))
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
               (list (cons (config-files-path "gatus/config.yaml") "/config/config.yaml")
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
       (simple-service 'home-oci-victoria-logs
           home-oci-service-type
         (oci-extension
          ;; (networks
          ;;  (list
          ;;   (oci-network-configuration
          ;;    (name "grafana-network"))))
          (containers
           (list
            (oci-container-configuration
              (provision "victoria-logs")
              (image "grafana/grafana:latest")
              (container-user "1000")
              (environment '())
              (volumes '(("/home/krisbalintona/services/grafana/data" . "/var/lib/grafana")))
              ;; (network "grafana-network")
              ;; (ports '("127.0.0.1:3000:3000"))
              (auto-start? #t)
              (respawn? #f))))))
       (simple-service 'home-restic-vault
           home-restic-backup-service-type
         (list
          (restic-job/defaults
           #:name "restic-vault"
           #:repository sops-sublation-repository-path
           #:schedule "0 7-22/3 * * *"
           #:files (list (string-append (getenv "HOME") "/vault")))))
       (simple-service 'home-restic-crowdsec
           home-restic-backup-service-type
         (list
          (restic-job/defaults
           #:name "restic-crowdsec"
           #:repository sops-sublation-repository-path
           #:schedule "0 0 */2 * *"
           #:files (list (string-append services-dir "/crowdsec")))))
       (simple-service 'home-restic-pocket-id
           home-restic-backup-service-type
         (list
          (restic-job/defaults
           #:name "restic-pocket-id"
           #:repository sops-sublation-repository-path
           #:schedule "0 0 */4 * *"
           #:files (list (string-append services-dir "/pocket-id/data")))))
       (simple-service 'home-restic-caddy
           home-restic-backup-service-type
         (list
          (restic-job/defaults
           #:name "restic-caddy"
           #:repository sops-sublation-repository-path
           #:schedule "0 0 */2 * *"
           #:files (list (string-append services-dir "/caddy")))))
       (simple-service 'home-restic-copyparty
           home-restic-backup-service-type
         (list
          (restic-job/defaults
           #:name "restic-copyparty"
           #:repository sops-sublation-repository-path
           #:schedule "0 12 * * *"
           #:files (list (string-append services-dir "/copyparty")))))
       (simple-service 'home-restic-vaultwarden
           home-restic-backup-service-type
         (list
          (restic-job/defaults
           #:name "restic-vaultwarden"
           #:repository sops-sublation-repository-path
           #:schedule "0 0 * * *"
           #:files (list (string-append services-dir "/vaultwarden")))))
       (service home-syncthing-service-type
         (let* (;; Folders
                (biblio-folder
                 (syncthing-folder
                   (id syncthing-biblio-folder-id)
                   (label "Biblio")
                   (path "~/services/copyparty/data/biblio")
                   (devices
                    (list syncthing-one-plus-7-pro-device)))))
           (for-home
            (syncthing-configuration
              (user "krisbalintona")
              (config-file
               (syncthing-config-file
                 (folders (list biblio-folder))))))))
       (simple-service 'files-Xdefaults
           home-files-service-type
         `((".Xdefaults" ,%default-xdefaults)))
       (service home-xdg-configuration-files-service-type
         `(("gdb/gdbinit" ,%default-gdbinit)
           ("nano/nanorc" ,%default-nanorc)))
       %base-home-services)))))
