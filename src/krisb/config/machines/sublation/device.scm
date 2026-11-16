(define-module (krisb config machines sublation device)
  #:use-module (krisb config common)
  #:export (%sops-age-key-file
            %syncthing-device))


;;
;;; SOPS
;;

(define %sops-age-key-file
  (let ((path "/home/krisbalintona/.config/sops/age/keys.txt"))
    (if (file-exists? path)
        path
        (error "Age key file not found:" path))))


;;
;;; Syncthing device
;;

(define-public %syncthing-device
  (syncthing-device
    (id "IEIGZYX-QJGTCMC-LTWQ6S4-2RB77GM-WOXUBMM-J26I7LO-KIROD5R-XP7LXAT")
    (name "Sublation (laptop server, Guix)")))
