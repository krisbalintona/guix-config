(define-module (krisb config machines Maur home)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services niri) ; 'home-niri-service-type'
  #:use-module (gnu home services syncthing) ; sy
  #:use-module (gnu packages base)      ; gnu-make
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)     ; tree
  #:use-module (gnu packages window-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages ssh)         ; x11-ssh-askpass
  #:use-module (gnu packages vim)         ; neovim
  #:use-module (gnu packages rust-apps)   ; jujutsu
  #:use-module (gnu packages shellutils)  ; direnv
  #:use-module (gnu packages freedesktop) ; xdg-utils
  #:use-module (gnu packages music)       ; playerctl
  #:use-module (gnu packages compression) ; zip
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (nongnu packages mozilla) ; Firefox
  #:use-module (krisb home services audio)
  #:use-module (krisb home services netbird)
  #:use-module (krisb config machines features vcs)
  #:use-module (krisb config machines Maur features auth)
  #:use-module (krisb config machines Maur features syncthing)
  #:export (Maur-home-environment))

(define Maur-home-environment
  (home-environment
    (packages (list git
                    x11-ssh-askpass
                    neovim
                    jujutsu
                    fd
                    tree
                    noctalia-shell
                    playerctl
                    wlr-randr
                    xdg-utils
                    direnv
                    firefox
                    gnu-make
                    zip
                    unzip
                    tar
                    btop
                    ripgrep))
    (services
     (append (feature-openssh)
             (feature-syncthing)
             (feature-git)
             (list (service home-netbird-ui-service-type
                     (home-netbird-ui-configuration
                      (respawn? #f)))
                   (service home-easyeffects-service-type
                     (home-easyeffects-configuration
                      (respawn? #f)))

                   (service home-niri-service-type)
                   (service home-pipewire-service-type)
                   ;; The 'home-wayland-service-type' provides the
                   ;; 'wayland-display' Shepherd service, whose value
                   ;; is used by other services to determine the value
                   ;; of the WAYLAND_DISPLAY env var.  Nothing I have
                   ;; currently instantiates it, so I instantiate it
                   ;; myself.  If in the future I use a service-type
                   ;; that does instantiate it, I can delete this one.
                   (service home-wayland-service-type 30)

                   (service home-bash-service-type)
                   (service home-fish-service-type)

                   (service home-files-service-type
                     `((".guile" ,%default-dotguile)
                       (".Xdefaults" ,%default-xdefaults)))
                   (service home-xdg-configuration-files-service-type
                     `(("gdb/gdbinit" ,%default-gdbinit)
                       ("nano/nanorc" ,%default-nanorc))))
             %base-home-services))))
