(define-module (krisb config machines Maur device)
  #:use-module (gnu services syncthing)
  #:export (%syncthing-device))


;;
;;; Syncthing device
;;

(define %syncthing-device
  (syncthing-device
    (id "JGMFYQZ-HTPMS7U-G42UZ7L-NHOW7FE-OKNUUOS-MIWVBAC-3ZTE5S3-F7IPCQ6")
    (name "Maur (FW13 2026, Guix System))")))
