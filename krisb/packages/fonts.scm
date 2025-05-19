(define-module (krisb packages fonts)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system font)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download))

(define-public font-iosevka-nerd-font
  (package
   (name "font-iosevka-nerd-font")
   (version "33.2.2")
   (source
    (origin
     (method url-fetch/zipbomb)
     (uri (string-append
           "https://github.com/Iosevka-NerdFont/Iosevka"
           "/archive/refs/tags/"
           "v" version ".zip"))
     (sha256
      (base32 "157grgaz5l272pn0lj0021dj3f50q2handl03r640pvabj5paw1f"))))
   (build-system font-build-system)
   (home-page "https://github.com/Iosevka-NerdFont/Iosevka")
   (synopsis "Iosevka Nerd Font")
   (description "Iosevka font with nerd icons.")
   (license (list license:silofl1.1))))

(define-public font-iosevka-aile-nerd-font
  (package
   (name "font-iosevka-aile-nerd-font")
   (version "33.2.2")
   (source
    (origin
     (method url-fetch/zipbomb)
     (uri (string-append
           "https://github.com/Iosevka-NerdFont/IosevkaAile"
           "/archive/refs/tags/"
           "v" version ".zip"))
     (sha256
      (base32 "1qi6b0fq0jplyb6ly2sz7kbcd1rv2qqw6vfiai7xbc8hmhb7v1y7"))))
   (build-system font-build-system)
   (home-page "https://github.com/Iosevka-NerdFont/IosevkaAile")
   (synopsis "Iosevka Aile Nerd Font")
   (description "Iosevka Aile font with nerd icons.")
   (license (list license:silofl1.1))))

(define-public font-iosevka-term-ss04-nerd-font
  (package
   (name "font-iosevka-term-ss04-nerd-font")
   (version "33.2.2")
   (source
    (origin
     (method url-fetch/zipbomb)
     (uri (string-append
           "https://github.com/Iosevka-NerdFont/IosevkaTermSS04"
           "/archive/refs/tags/"
           "v" version ".zip"))
     (sha256
      (base32 "199wkjv24jxxpbnx2wk76ayghj1yslk214lx619qk5agcqs1irjd"))))
   (build-system font-build-system)
   (home-page "https://github.com/Iosevka-NerdFont/IosevkaTermSS04")
   (synopsis "Iosevka Term SS04 Nerd Font")
   (description "Iosevka Term SS04 font with nerd icons.")
   (license (list license:silofl1.1))))

(define-public font-iosevka-ss11-nerd-font
  (package
   (name "font-iosevka-ss11-nerd-font")
   (version "33.2.2")
   (source
    (origin
     (method url-fetch/zipbomb)
     (uri (string-append
           "https://github.com/Iosevka-NerdFont/IosevkaSS11"
           "/archive/refs/tags/"
           "v" version ".zip"))
     (sha256
      (base32 "093g0qs95zrfyaf0bnd91z2bwqbyhnj5haygnbkhplyd6dap1xqw"))))
   (build-system font-build-system)
   (home-page "https://github.com/Iosevka-NerdFont/IosevkaSS11")
   (synopsis "Iosevka SS11 Nerd Font")
   (description "Iosevka SS11 font with nerd icons.")
   (license (list license:silofl1.1))))

(define-public font-iosevka-term-ss11-nerd-font
  (package
   (name "font-iosevka-term-ss11-nerd-font")
   (version "33.2.2")
   (source
    (origin
     (method url-fetch/zipbomb)
     (uri (string-append
           "https://github.com/Iosevka-NerdFont/IosevkaTermSS11"
           "/archive/refs/tags/"
           "v" version ".zip"))
     (sha256
      (base32 "1spljlincy5xqbs5jrjafwcdnpzy9xpgr1lr4m5mw2f9xb91rln2"))))
   (build-system font-build-system)
   (home-page "https://github.com/Iosevka-NerdFont/IosevkaTermSS11")
   (synopsis "Iosevka Term SS11 Nerd Font")
   (description "Iosevka Term SS11 font with nerd icons.")
   (license (list license:silofl1.1))))

(define-public font-overpass-nerd-font
  (package
   (name "font-overpass-nerd-font")
   (version "3.4.0")
   (source
    (origin
     (method url-fetch/zipbomb)
     (uri (string-append
           "https://github.com/ryanoasis/nerd-fonts"
           "/releases/download/"
           "v" version "/Overpass.zip"))
     (sha256
      (base32 "0hxqxr5yswj22dwk14l328imfqnyjw4llwhidjmqc120rv0s9r9r"))))
   (build-system font-build-system)
   (home-page "https://github.com/ryanoasis/nerd-fonts")
   (synopsis "Overpass Nerd Font")
   (description "Overpass font with nerd icons.")
   (license (list license:silofl1.1))))

(define-public font-jetbrains-mono-nerd-font
  (package
   (name "font-jetbrains-mono-nerd-font")
   (version "3.4.0")
   (source
    (origin
     (method url-fetch/zipbomb)
     (uri (string-append
           "https://github.com/ryanoasis/nerd-fonts"
           "/releases/download/"
           "v" version "/JetBrainsMono.zip"))
     (sha256
      (base32 "0g29gj9d6720grfr2vasnvdppzw4hycpfyd5di54d2p4mkrmzw3n"))))
   (build-system font-build-system)
   (home-page "https://github.com/ryanoasis/nerd-fonts")
   (synopsis "JetBrains Mono Nerd Font")
   (description "JetBrains Mono font with nerd icons.")
   (license (list license:silofl1.1))))
