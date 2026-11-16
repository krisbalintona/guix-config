(define-module (krisb config machines common syncthing)
  #:use-module (gnu services syncthing)
  #:export (%one-plus-7-pro-device

            %notes-folder-id
            %agenda-folder-id
            %biblio-folder-id

            %syncthing-gui-safe-config))


;;
;;; Devices
;;
;; For devices whose,

(define %one-plus-7-pro-device
    (syncthing-device
      (id "OVGYOBF-JPFQJKE-6CKRY7J-JULRCWK-WSGSA6Y-SQZYLLE-B2OLSDJ-6DRSTQZ")
      (name "OnePlus 7 Pro")))


;;
;;; Folders
;;

(define %notes-folder-id "qtuzy-ufufb")

(define %agenda-folder-id "k4vqh-rny7b")

(define %biblio-folder-id "kjtm2-zyajn")


;;
;;; Helpers
;;

(define %syncthing-gui-safe-config
  (syncthing-config-file
    ;; Don't have any unacknowledged notifications, since otherwise,
    ;; dismissing these notifications in the GUI will cause the actual
    ;; configuration to differ from the generated one, leading to the
    ;; config.xml getting replaced (and backed up in $HOME) during the
    ;; next reconfigure.
    (unacked-notification-id "")
    ;; Accept the usage reporting prompt so interacting with it in the
    ;; GUI doesn't change the config file.
    (usage-reporting-accepted 1)
    (usage-reporting-seen 1))) ; Should be the same as 'usage-reporting-accepted'?
