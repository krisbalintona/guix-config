(define-module (krisb config machines Maur features syncthing)
  #:use-module (gnu services)
  #:use-module (gnu home services syncthing)
  #:use-module ((krisb config machines common syncthing) #:prefix common:)
  #:use-module ((krisb config machines mute device) #:prefix mute:)
  #:export (feature-syncthing))

(define (feature-syncthing)
  (list (service home-syncthing-service-type
          (let* (;; Folders
                 (agenda-folder
                  (syncthing-folder
                    (id common:%agenda-folder-id)
                    (label "Agenda")
                    (path "~/Documents/org-database/agenda/")
                    (devices
                     (list mute:%syncthing-device
                           common:%one-plus-7-pro-device))))
                 (notes-folder
                  (syncthing-folder
                    (id common:%notes-folder-id)
                    (label "Notes")
                    (path "~/Documents/org-database/notes")
                    (devices
                     (list mute:%syncthing-device
                           common:%one-plus-7-pro-device)))))
            (for-home
             (syncthing-configuration
               (user "krisbalintona")
               (config-file
                (syncthing-config-file
                  (folders (list agenda-folder notes-folder))))))))))
