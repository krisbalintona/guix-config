(define-module (krisb config machines sublation features syncthing)
  #:use-module (gnu services)
  #:use-module (gnu home services syncthing)
  #:use-module ((krisb config machines common syncthing) #:prefix common:)
  #:export (feature-syncthing))

(define (feature-syncthing)
  (list (service home-syncthing-service-type
          (let* (;; Folders
                 (biblio-folder
                  (syncthing-folder
                    (id common:%biblio-folder-id)
                    (label "Biblio")
                    (path "~/services/copyparty/data/biblio")
                    (devices (list common:%one-plus-7-pro-device)))))
            (for-home
             (syncthing-configuration
               (user "krisbalintona")
               (config-file
                (syncthing-config-file
                  (inherit common:%syncthing-gui-safe-config)
                  (folders (list biblio-folder))))))))))
