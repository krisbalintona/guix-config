(define-module (krisb config machines Maur features sound)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages music)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:export (feature-easyeffects))

(define (feature-easyeffects)
  (list (simple-service 'easyeffects-audio-effects-packages
            home-profile-service-type
          ;; Install EasyEffects along with these external plugin
          ;; packages, which provide various effects:
          ;; - Linux Studio Plugins (lsp-plugins)
          ;; - Calf Studio Gear (calf)
          (list easyeffects lsp-plugins calf))
        (service home-easyeffects-service-type)))

