(define-module (krisb config machines common syncthing)
  #:use-module (gnu services syncthing)
  #:export (%one-plus-7-pro-device

            %notes-folder-id
            %agenda-folder-id
            %biblio-folder-id))


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
