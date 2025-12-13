;; This is a sample Guix Home configuration which can help setup your
;; home directory in the same declarative manner as Guix System.
;; For more information, see the Home Configuration section of the manual.
(define-module (guix-home-config)
  #:use-module (guix gexp)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services containers)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services containers)
  #:use-module (gnu packages python))

(define home-config
  (home-environment
    (packages
     (specifications->packages
      (list
       "vim"
       "neovim"
       "ripgrep"
       "tree"
       "jujutsu"
       "parted"
       "rsync")))
    
    (services
     (cons* (service home-oci-service-type
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
                  (subnet "10.42.0.0/24")
                  (internal? #t))))
               (containers
                (list
                 (oci-container-configuration
                   (provision "caddy")
                   (image "docker.io/caddy:2.10.2")
                   (network "contained-network")
                   ;; Some of the settings below were copied from the
                   ;; template/starter Docker usage described on the
                   ;; home page of the Docker image used above:
                   ;; https://hub.docker.com/_/caddy
                   (ports '(("80" . "80")
                            ("443" . "443")
                            ("443" . "443/udp")))
                   (volumes
                    `(("caddy_data" . "/data")
                      ("caddy_log" . "/data/log")
                      (,(string-append (dirname (current-filename)) "/files/caddy/Caddyfile")
                       . "/config/Caddyfile")))
                   (command '("caddy" "run" "--config" "/config/Caddyfile"))
                   (auto-start? #t)
                   (respawn? #f))
                 (oci-container-configuration
                   (provision "copyparty-server")
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
            (service home-fish-service-type)
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
