(define-module (krisb config machines mute features fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:export (feature-fonts))

(define (feature-fonts)
  (list
   ;; For font CLI utilities
   (simple-service 'fontconfig
       home-profile-service-type
     (list fontconfig))
   ;; Font for terminal
   (simple-service 'fontconfig
       home-profile-service-type
     (list font-iosevka))))
