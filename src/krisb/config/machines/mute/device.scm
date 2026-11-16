(define-module (krisb config machines mute device)
  #:use-module (gnu services syncthing)
  #:export (%syncthing-device))


;;
;;; Syncthing device
;;

(define-public %syncthing-device
  (syncthing-device
    (id "UIVVWP3-XZ2GV2F-7N3RMXC-FRIKYXP-Y2MAXNA-JMGD45T-AZYZZJC-FBFSQQ5")
    (name "Mute (G14 2024, Guix in Arch))")))
