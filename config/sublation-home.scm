(use-modules (guix gexp)
             (gnu packages)
             (gnu system shadow)        ; For user-group 
             (gnu home services)
             (gnu home services shepherd)
             (gnu home services shells)
             (gnu home services shells)
             (abbe packages rust)
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
             (gnu services containers)
             (gnu home services shepherd)
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
(define services-dir
  (string-append (getenv "HOME") "/services"))
(define* (restic-job/defaults
          #:key
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
          (extra-flags '())
          (verbose? #t))
  (restic-backup-job
    (name name)
    (files files)
    (schedule schedule)
    (repository repository)
    (password-file password-file)
    (extra-flags extra-flags)
    (verbose? verbose?)))

(home-environment
  (packages
   (specifications->packages
    (list
     "glibc-locales"
     "git"
     "make"
     "vim"
     "neovim"
     "tree"
     "ripgrep"
     "parted"
     "emacs"
     "brightnessctl"
     "jujutsu"
     "btop"
     "gnupg"
     "age"
     "bind:utils"
     "unbound"
     "restic")))
  
  (services
   (cons*
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
            "if [[ $- == *i* ]] && { [[ ! $TERM =~ dumb ]] || [[ $TERM =~ eat ]]; }; then
        exec fish
    fi")))))
    (service home-fish-service-type
      (home-fish-configuration
        (config
         (list (plain-file "fish_greeting.fish" "set -g fish_greeting")))))
    (simple-service 'krisb-symlink-jj-config-files-service-type
          home-xdg-configuration-files-service-type
        `(("jj/config.toml"
           ,(local-file "files/jujutsu/config.toml"))))
    (service home-gpg-agent-service-type)
    (service home-sops-secrets-service-type
      (home-sops-service-configuration
        (config (local-file "files/sops/sops.yaml" "sops.yaml"))
        (secrets
         (list
          (sops-secret
            (key '("pihole-webserver-password"))
            (file (local-file "files/sops/sublation.yaml"))
            (permissions #o400))
          (sops-secret
            (key '("netlify-access-token"))
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
    (simple-service 'home-oci-pihole
        home-oci-service-type
      (oci-extension
       (networks
        (list
         (oci-network-configuration
          (name "pihole-network"))))
       (containers
        (list
         (let* ((pihole-password-filename "pihole-webserver-password")
                (pihole-password-sops-file
                 (string-append "/run/user/" (number->string (getuid)) "/secrets"
                                "/" pihole-password-filename))
                (pihole-password-file
                 (string-append "/run/secrets/" pihole-password-filename)))
           (oci-container-configuration
             (provision "pihole")
             (image "docker.io/pihole/pihole:latest")
             (environment
              `("TZ=America/Chicago"
                "FTLCONF_dns_port=53" ; Default port
                ;; Listen for queries on all interfaces and from all
                ;; origins
                "FTLCONF_dns_listeningMode=ALL"
                ;; Forward queries to Unbound DNS (whose port is 5335 on
                ;; the host)
                "FTLCONF_dns_upstreams=host.containers.internal#5335"
                ;; Pihole as NTP server for other devices?
                "FTLCONF_ntp_ipv4_active=false"
                "FTLCONF_ntp_ipv6_active=false"
                ;; Pihole as NTP server for host?
                "FTLCONF_ntp_sync_active=false"
                ;; Webserver settings
                "FTLCONF_webserver_port=8080"
                ,(cons "WEBPASSWORD_FILE" pihole-password-file)))
             (network "pihole-network")
             (ports '(;; DNS queries
                      "53:53/tcp"
                      "53:53/udp"
                      ;; Webserver
                      "127.0.0.1:8080:8080"))
             (volumes
              `(("/home/krisbalintona/services/pihole/data" . "/etc/pihole")
                ,(cons pihole-password-sops-file pihole-password-file)))
             (auto-start? #t)
             (respawn? #f)))))))
    (simple-service 'home-oci-caddy
        home-oci-service-type
      (oci-extension
       (containers
        (list
         (oci-container-configuration
           (provision "caddy")
           (image
             (oci-image
               ;; OCI images locations follow a
               ;; [registry]/[repository]:[tag] format.  Since we are
               ;; local, we only need to specify a repository and
               ;; optionally a tag.  The REPOSITORY field below
               ;; corresponds to the [repository] of an OCI image
               ;; location; it can be whatever we want since this image is
               ;; created locally (in the Guix store)
               (repository "caddy-netlify")
               (tag "2.10.2")
               (value (specifications->manifest '("coreutils"
                                                  "caddy-netlify")))
               (pack-options '(#:symlinks (("/bin" -> "bin"))))))
           ;; These environment variables are set in the Docker image
           ;; Caddy distributes (shown by e.g. "podman image inspect
           ;; docker.io/caddy:2.10.2").  My tests show that they need to
           ;; be set for some reason
           (environment '("CADDY_VERSION=v2.10.2"
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
            `(("/home/krisbalintona/services/caddy/data" . "/data") ; Path of XDG_DATA_HOME
              ("/home/krisbalintona/services/caddy/log" . "/data/log")
              (,(string-append (dirname (current-filename)) "/files/caddy/Caddyfile")
               . "/config/Caddyfile")
              ;; Netlify access token
              (,(string-append "/run/user/" (number->string (getuid)) "/secrets"
                               "/netlify-access-token")
               ;; Reference this with a file placeholder in my Caddyfile;
               ;; see
               ;; https://caddyserver.com/docs/conventions#placeholders
               . "/run/secrets/netlify-access-token")
              ;; Goaccess real-time web page
              ("goaccess_web" . "/var/www/goaccess")
              ,(cons (string-append (getenv "XDG_RUNTIME_DIR") "/copyparty")
                     "/run/copyparty")))
           (command '("caddy" "run" "--config" "/config/Caddyfile"))
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
                      "--ws-url=wss://goaccess.home.arpa:443/ws"
                      "--port=7890"
                      "--tz='America/Chicago'"))
           (auto-start? #t)
           (respawn? #f))))))
    (simple-service 'home-oci-copyparty-socket
        home-shepherd-service-type
      (list
       (shepherd-service
         (provision '(home-oci-copyparty-socket))
         (one-shot? #t)
         (start
          #~(lambda ()
              (let ((dir (string-append (getenv "XDG_RUNTIME_DIR") "/copyparty")))
                (mkdir-p dir)
                (format #t "Socket directory exists at: ~a~%" dir)
                #t)))
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
            `(,(cons (string-append (getenv "XDG_RUNTIME_DIR") "/copyparty")
                     "/run/copyparty")
              ("/home/krisbalintona/services/copyparty/data" . "/data")
              (,(string-append (dirname (current-filename)) "/files/copyparty/copyparty.conf")
               . "/srv/copyparty.conf")))
           (command '("-c" "/srv/copyparty.conf" "--chdir" "/srv"))
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
            `("ADMIN_TOKEN='$argon2id$v=19$m=65540,t=3,p=4$T4YVDSINU+4alWZ22logYyqgUbQn4J4o2DAW/deZF3o$zKnbtTREy8wxrCDEAne1F58/CXBHRiKkII9NqsoGVJA'"
              "DOMAIN=https://vault.kristofferbalintona.me"
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
    (service home-restic-backup-service-type)
    (simple-service 'home-restic-vault
        home-restic-backup-service-type
      (list
       (restic-job/defaults
        #:name "restic-vault"
        #:schedule "0 7-22/3 * * *"
        #:files (list (string-append (getenv "HOME") "/vault")))))
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
    
    (simple-service 'guix-locales
        home-environment-variables-service-type
      '(("GUIX_LOCPATH" . "$HOME/.guix-profile/lib/locale")))
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
