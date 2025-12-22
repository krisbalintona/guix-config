;; This is a sample Guix Home configuration which can help setup your
;; home directory in the same declarative manner as Guix System.
;; For more information, see the Home Configuration section of the manual.
(define-module (guix-home-config)
  #:use-module (guix gexp)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (gnu services containers)
  #:use-module (gnu services backup)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services containers)
  #:use-module (gnu home services backup)
  #:use-module (gnu home services gnupg)
  #:use-module (sops secrets)
  #:use-module (sops home services sops)
  #:use-module (abbe packages rust))    ; For Jujutsu

(define home-config
  (home-environment
    (packages
     (specifications->packages
      (list
       "glibc-locales"
       "vim"
       "neovim"
       "ripgrep"
       "tree"
       "jujutsu"                        ; From Abbe channel
       "parted"
       "rsync"
       "age"
       "gnupg")))
    
    (services
     (cons* (service home-restic-backup-service-type
              (let ((restic-repository "/mnt/backup-hdd")
                    (restic-password-file
                     (string-append "/run/user/" (number->string (getuid)) "/secrets"
                                    "/restic-backup-password")))
                (restic-backup-configuration
                  (jobs
                   (list
                    (restic-backup-job
                      (name "restic-copyparty-data")
                      (repository restic-repository)
                      (password-file restic-password-file)
                      (schedule "0 5 * * *")
                      (files (list (string-append (getenv "HOME") "/copyparty-data")))
                      (verbose? #t))
                    (restic-backup-job
                      (name "restic-vault")
                      (repository restic-repository)
                      (password-file restic-password-file)
                      (schedule "0 7-22/3 * * *")
                      (files (list (string-append (getenv "HOME") "/vault")))
                      (verbose? #t)))))))
            (service home-sops-secrets-service-type
              (home-sops-service-configuration
                (config (local-file "files/sops/sops.yaml" "sops.yaml"))
                (secrets
                 (list
                  (sops-secret
                    (key '("restic-backup-password"))
                    (file (local-file "files/sops/sublation.yaml"))
                    (permissions #o400))
                  (sops-secret
                    (key '("netlify-access-token"))
                    (file (local-file "files/sops/sublation.yaml"))
                    (permissions #o400))
                  (sops-secret
                    (key '("pihole-webserver-password"))
                    (file (local-file "files/sops/sublation.yaml"))
                    (permissions #o400))
                  (sops-secret
                    (key '("wireguard-private-key"))
                    (file (local-file "files/sops/sublation.yaml"))
                    (permissions #o400))))))
            (service home-gpg-agent-service-type)
            (service home-oci-service-type
              (for-home
               (oci-configuration
                (runtime 'podman)       ; Use podman instead of docker
                (verbose? #t))))
            (simple-service 'home-oci-services
                home-oci-service-type
              (oci-extension
               (networks
                (list
                 (oci-network-configuration
                  (name "contained-network")
                  (subnet "10.42.0.0/24"))))
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
                        ;; Listen for queries on all interfaces and
                        ;; from all origins
                        "FTLCONF_dns_listeningMode=ALL"
                        ;; Forward queries to Unbound DNS (whose port
                        ;; is 5335 on the host; we're using the host
                        ;; network in this container so its IP is
                        ;; 127.0.0.1)
                        "FTLCONF_dns_upstreams=127.0.0.1#5335"
                        ;; Pihole as NTP server for other devices?
                        "FTLCONF_ntp_ipv4_active=false"
                        "FTLCONF_ntp_ipv6_active=false"
                        ;; Pihole as NTP server for host?
                        "FTLCONF_ntp_sync_active=false"
                        ;; Webserver settings
                        "FTLCONF_webserver_port=8080"
                        ,(cons "WEBPASSWORD_FILE" pihole-password-file)))
                     (network "host")
                     (volumes `(("pihole-etc" . "/etc/pihole")
                                ,(cons pihole-password-sops-file pihole-password-file)))
                     (auto-start? #t)
                     (respawn? #f)))
                 (oci-container-configuration
                   (provision "caddy")
                   (image
                     (oci-image
                       ;; OCI images locations follow a
                       ;; [registry]/[repository]:[tag] format.  Since
                       ;; we are local, we only need to specify a
                       ;; repository and optionally a tag.  The
                       ;; REPOSITORY field below corresponds to the
                       ;; [repository] of an OCI image location; it
                       ;; can be whatever we want since this image is
                       ;; created locally (in the Guix store)
                       (repository "caddy-netlify")
                       (tag "2.10.2")
                       (value (specifications->manifest '("caddy-netlify")))
                       (pack-options '(#:symlinks (("/bin" -> "bin"))))))
                   ;; These environment variables are set in the
                   ;; Docker image Caddy distributes (shown by
                   ;; e.g. "podman image inspect
                   ;; docker.io/caddy:2.10.2").  My tests show that
                   ;; they need to be set for some reason
                   (environment '("CADDY_VERSION=v2.10.2"
                                  "XDG_CONFIG_HOME=/config"
                                  "XDG_DATA_HOME=/data"))
                   (network "contained-network")
                   (ports '(("80" . "80")
                            ("443" . "443")
                            ("443" . "443/udp")))
                   (volumes
                    `(("caddy_data" . "/data")
                      ("caddy_log" . "/data/log")
                      (,(string-append (dirname (current-filename)) "/files/caddy/Caddyfile")
                       . "/config/Caddyfile")
                      ;; Netlify access token
                      (,(string-append "/run/user/" (number->string (getuid)) "/secrets"
                                       "/netlify-access-token")
                       ;; Reference this with a file placeholder in my
                       ;; Caddyfile; see
                       ;; https://caddyserver.com/docs/conventions#placeholders
                       . "/run/secrets/netlify-access-token")
                      ;; Goaccess real-time web page
                      ("goaccess_web" . "/var/www/goaccess")))
                   (command '("caddy" "run" "--config" "/config/Caddyfile"))
                   (auto-start? #t)
                   (respawn? #f))
                 (oci-container-configuration
                   (provision "goaccess")
                   (image
                     (oci-image
                       (repository "goaccess")
                       (tag "1.9.3")
                       (value (specifications->manifest '("goaccess")))
                       (pack-options '(#:symlinks (("/bin" -> "bin"))))))
                   (network "contained-network")
                   (volumes `(("caddy_log" . "/var/log")
                              ("goaccess_web" . "/var/www/goaccess")))
                   ;; Command taken from here:
                   ;; https://dev.to/emrancu/setup-goaccess-in-ubuntulinux-with-docker-and-real-cad-access-over-domainsub-domain-226n
                   ;;
                   ;; Goaccess uses websockets for real-time updates.
                   ;; We can confirm that the goaccess page we see is
                   ;; receiving real-time updates from the green dot
                   ;; on the top left of the page (beside the burger
                   ;; menu icon).
                   ;;
                   ;; Caddy directs requests to our nselected domain
                   ;; to the HTTPS port (443) of goaccess's network
                   ;; (i.e., the container network).  As such, Caddy
                   ;; expects the goaccess's websocket to be at
                   ;; wss://DOMAIN:443/ws.
                   ;; 
                   ;; (And we don't have to worry about passing SSL
                   ;; information to the goaccess invocation
                   ;; certificates with the "tls internal" Caddy
                   ;; setting.)
                   ;;
                   ;; Caddy just listens to the websocket to know when
                   ;; to update the files it serves, but the actual
                   ;; file it serves is at a path accessible in its
                   ;; container.  Caddy then knows to just serve these
                   ;; files via the "file_server" setting.
                   (command '("goaccess"
                              "/var/log/copyparty-json.log"
                              "--log-format=CADDY"
                              "-o" "/var/www/goaccess/index.html"
                              "--real-time-html"
                              "--ws-url=wss://goaccess.home.arpa:443/ws"
                              "--port=7890"
                              "--tz='America/Chicago'"))
                   (auto-start? #t)
                   (respawn? #f))
                 (oci-container-configuration
                   (provision "copyparty")
                   (image "docker.io/copyparty/ac:1.19.21")
                   (network "contained-network")
                   ;; Have files mounted at /data/ and copyparty
                   ;; config + cache files in /srv/
                   (volumes
                    `(("/home/krisbalintona/copyparty-data" . "/data")
                      (,(string-append (dirname (current-filename)) "/files/copyparty/copyparty.conf")
                       . "/srv/copyparty.conf")))
                   (command '("-c" "/srv/copyparty.conf" "--chdir" "/srv"))
                   (auto-start? #t)
                   (respawn? #f))))))
            (service home-bash-service-type
              (home-bash-configuration
                (bashrc
                 (list
                  ;; 2025-12-06: My default shell is bash because I
                  ;; haven't gotten Tramp to work with Fish shell
                  ;; yet.  (I've deduced that the prompt is not the
                  ;; problem.)  So, instead, I keep bash as my
                  ;; shell and dispatch to fish if the current
                  ;; process wasn't started by tramp
                  (plain-file "to-fish.bash"
                    "if [[ $- == *i* ]] && { [[ ! $TERM =~ dumb ]] || [[ $TERM =~ eat ]]; }; then
    exec fish
fi")))))
            (service home-fish-service-type
              (home-fish-configuration
                (config
                 (list (plain-file "fish_greeting.fish" "set -g fish_greeting")))))
            (simple-service 'guix-locales
                home-environment-variables-service-type
              '(("GUIX_LOCPATH" . "$HOME/.guix-profile/lib/locale")))
            (simple-service 'krisb-symlink-git-config-files-service-type
                home-xdg-configuration-files-service-type
              `(("git/config"
                 ,(local-file "files/git/config"))))
            (simple-service 'krisb-symlink-jj-config-files-service-type
                home-xdg-configuration-files-service-type
              `(("jj/config.toml"
                 ,(local-file "files/jujutsu/config.toml"))))
            (service home-openssh-service-type
              (home-openssh-configuration
                (hosts
                 (list
                  (openssh-host (name "WindowsG14")
                                (host-name "192.168.4.138")
                                (user "krisbalintona")
                                (forward-x11? #t)
                                (forward-x11-trusted? #t))))))

            (service home-files-service-type
              `((".guile" ,%default-dotguile)
                (".Xdefaults" ,%default-xdefaults)))

            (service home-xdg-configuration-files-service-type
              `(("gdb/gdbinit" ,%default-gdbinit)
                ("nano/nanorc" ,%default-nanorc)))

            %base-home-services))))

home-config
