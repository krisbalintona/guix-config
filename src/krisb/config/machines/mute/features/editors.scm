(define-module (krisb config machines mute features editors)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (krisb packages fonts)
  #:export (feature-editors))

(define (feature-editors)
  (list (simple-service 'editor-packages
            home-profile-service-type
          (list nano
                neovim
                emacs))
        ;; Fonts used in Emacs
        (simple-service 'emacs-font-packages
            home-profile-service-type
          (list font-google-noto-emoji  ; For emojis
                font-iosevka
                font-iosevka-aile-nerd-font
                font-iosevka-term-ss04-nerd-font
                font-iosevka-ss11
                font-iosevka-ss11-nerd-font
                font-iosevka-term-ss11-nerd-font
                font-overpass-nerd-font
                font-jetbrains-mono-nerd-font
                font-adobe-source-sans
                font-adobe-source-serif))))
