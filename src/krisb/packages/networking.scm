(define-module (krisb packages networking)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (abbe packages caddy)
  #:export (nftables-geoip
            caddy-netlify
            caddy-netlify-coraza
            geolite2-country-mmdb
            geolite2-city-mmdb
            caddy-netlify-coraza-maxmind
            caddy-security-netlify-coraza-maxmind
            caddy-security-netlify-crowdsec-coraza-maxmind))

(define geoip-address-csv
  (package
    (name "geoip-address-csv")
    (version "2025-12")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://download.db-ip.com/free/dbip-country-lite-"
                       version ".csv.gz"))
       (sha256
        (base32 "0zm3kl68gf46a9xhvcsbzx7bpcxisvy1bk2khj0l5mj9zl0m2wpl"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder #~(begin
                    (use-modules (guix build utils))
                    (mkdir-p %output)
                    
                    (let ((file "/dbip.csv.gz"))
                      (copy-file #$source
                                 (string-append %output file))
                      ;; Decompress in place
                      (invoke (string-append #$gzip "/bin/gunzip")
                              (string-append %output file)))
                    #t)))
    (native-inputs (list gzip))
    (synopsis "DB-IP GeoIP CSV")
    (home-page "https://download.db-ip.com/free/")
    (description "DB-IP GeoIP country CSV")
    (license license:cc-by-sa4.0)))

(define nftables-geoip
  (package
    (name "nftables-geoip")
    (version "0")                       ; Upstream has no releases
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pvxe/nftables-geoip.git")
              (commit "252b1ac57059b35e4d286e0a0d06a5da77bcd743")))
       (sha256
        (base32 "136war4liaiyw3153a4cmlxvxagz8ld7sabw176371pqa0pcv5aq"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "python" "nft_geoip.py"
                      "--file-location" "location.csv"
                      "--file-address"
                      (string-append (assoc-ref inputs "geoip-address-csv")
                                     "/dbip.csv"))))
          (delete 'check)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (dir (string-append out "/etc/nftables")))
                (for-each
                 (lambda (f)
                   (install-file f dir))
                 (find-files "." "\\.nft$"))
                #t))))))
    (native-inputs
     (list python python-requests geoip-address-csv))
    (home-page "https://github.com/pvxe/nftables-geoip")
    (synopsis "Generate nftables GeoIP maps")
    (description
     "Generate nftables GeoIP rulesets from public IP geolocation data.")
    (license license:gpl2)))

;; NOTE 2025-12-27: The Caddy module path URLs are Go module paths.
;; This means, for instance, paths must have major version suffixes; I
;; learned this from
;; https://github.com/corazawaf/coraza-caddy/issues/214.  See
;; https://go.dev/ref/mod#major-version-suffixes for more information

(define caddy-netlify
  (let ((pkg (caddy-custom   ; From the Abbe channel; very convenient!
              "2.10.2"
              '("github.com/caddy-dns/netlify")
              "12a58yrwirvvzjr8ap9jx2v1afqnyfzikmi4kzbnncnir7a7rz0l"
              "1mfzxnlnw4sh0wn44mkmaz2dwhgb4rkwn0iz4nhhxprwn9krp51v")))
    (package/inherit pkg
      (name "caddy-netlify")
      (synopsis "Caddy with Netlify DNS support"))))

(define caddy-netlify-coraza
  (let ((pkg (caddy-custom
              "2.10.2"
              '("github.com/caddy-dns/netlify"
                "github.com/corazawaf/coraza-caddy/v2")
              "038n6y9izd4r0vk6bxkgkqc3kzkx6xisyb6f53w8kg205jc0wycd"
              "1qxg0cx5si8wndh36vxg13kp382k06zljm8j2q5sam2bfhbckmg3")))
    (package/inherit pkg
      (name "caddy-netlify-coraza")
      (synopsis "Caddy with Netlify DNS and Coraza WAF"))))

(define-public geolite2-country-mmdb
  (package
    (name "geolite2-country-mmdb")
    (version "e5f3a09776ddb0b215d20afefc31272e9b98d357")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/wp-statistics/GeoLite2-Country")
          (commit version)))
       (sha256
        (base32 "0yrd8s5x3cmsla9d3c79i1nv46nx5pzbr8mi5jn9ikab0yg7ph22"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder #~(begin
                    (use-modules (guix build utils))
                    (let* ((dir (string-append %output "/var/lib/geoip"))
                           (gz (string-append dir "/GeoLite2-Country.mmdb.gz")))
                      (mkdir-p dir)
                      (copy-file
                       (string-append #$source "/GeoLite2-Country.mmdb.gz")
                       gz)
                      ;; MaxMind database file at
                      ;; %output/var/lib/geoip/GeoLite2-Country.mmdb
                      (invoke (string-append #$gzip "/bin/gunzip") gz))
                    #t)))
    (native-inputs (list gzip))
    (synopsis "GeoLite2 Country MMDB (WP-Statistics redistribution)")
    (description
     "MaxMind-compatible GeoLite2 Country MMDB database redistributed
by WP-Statistics. Built from a pinned git commit.")
    (home-page "https://github.com/wp-statistics/GeoLite2-Country")
    (license license:cc-by-sa4.0)))

(define-public geolite2-city-mmdb
  (package
    (name "geolite2-city-mmdb")
    (version "b87c4ead4437c6593f6fd694afaffc7bad7085be")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/wp-statistics/GeoLite2-City.git")
          (commit version)))
       (sha256
        (base32 "11zzhx94b59r44y05c9zkmbdgjviwz9wcd5r25nm4d7qjmhmh64h"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder #~(begin
                    (use-modules (guix build utils))
                    (let* ((dir (string-append %output "/var/lib/geoip"))
                           (gz (string-append dir "/GeoLite2-City.mmdb.gz")))
                      (mkdir-p dir)
                      (copy-file
                       (string-append #$source "/GeoLite2-City.mmdb.gz")
                       gz)
                      (invoke (string-append #$gzip "/bin/gunzip") gz))
                    #t)))
    (native-inputs (list gzip))
    (synopsis "GeoLite2 City MMDB (WP-Statistics redistribution)")
    (description
     "MaxMind-compatible GeoLite2 City MMDB database redistributed
by WP-Statistics. Built from a pinned git commit.")
    (home-page "https://github.com/wp-statistics/GeoLite2-City")
    (license license:cc-by-sa4.0)))

(define caddy-netlify-coraza-maxmind
  (let ((pkg (caddy-custom
              "2.10.2"
              '("github.com/caddy-dns/netlify"
                "github.com/corazawaf/coraza-caddy/v2"
                "github.com/porech/caddy-maxmind-geolocation")
              "14smvz10kagxcs2mqi72pkfgi9aa6fva787fqid7x2xpydq5874b"
              "0zkncg08zjlaj0zcc4p8q0apmbczq4jym9plhlgk5mdlhmh203nr")))
    (package/inherit pkg
      (name "caddy-netlify-coraza-maxmind")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'install 'link-geolite-dbs
                ;; The city database is a superset of the country
                ;; database, but we bundle both: users may choose
                ;; which database to use
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (geoip-country (assoc-ref inputs "geolite2-country-mmdb"))
                         (geoip-city (assoc-ref inputs "geolite2-city-mmdb"))
                         (target (string-append out "/var/lib/geoip")))
                    (mkdir-p target)
                    (symlink
                     (string-append geoip-country "/var/lib/geoip/GeoLite2-Country.mmdb")
                     (string-append target "/GeoLite2-Country.mmdb"))
                    (symlink
                     (string-append geoip-city "/var/lib/geoip/GeoLite2-City.mmdb")
                     (string-append target "/GeoLite2-City.mmdb")))))))))
      (inputs
       (modify-inputs (package-inputs pkg)
         (append geolite2-country-mmdb geolite2-city-mmdb)))
      (synopsis "Caddy with Netlify DNS, Coraza WAF, and MaxMind geoblocking"))))

(define caddy-security-netlify-coraza-maxmind
  (let ((pkg (caddy-custom
              "2.10.2"
              '("github.com/greenpau/caddy-security"
                "github.com/caddy-dns/netlify"
                "github.com/corazawaf/coraza-caddy/v2"
                "github.com/porech/caddy-maxmind-geolocation")
              "14hydm9nn319nzm9g7ljnak4287p8ylv5npaxih8678yxwhsll3k"
              "1mfpf21w6r7lfhrdd99sasj9hbn74my83hqvf47qd3h7dlnxg6jj")))
    (package/inherit pkg
      (name "caddy-security-netlify-coraza-maxmind")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'install 'link-geolite-dbs
                ;; The city database is a superset of the country
                ;; database, but we bundle both: users may choose
                ;; which database to use
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (geoip-country (assoc-ref inputs "geolite2-country-mmdb"))
                         (geoip-city (assoc-ref inputs "geolite2-city-mmdb"))
                         (target (string-append out "/var/lib/geoip")))
                    (mkdir-p target)
                    (symlink
                     (string-append geoip-country "/var/lib/geoip/GeoLite2-Country.mmdb")
                     (string-append target "/GeoLite2-Country.mmdb"))
                    (symlink
                     (string-append geoip-city "/var/lib/geoip/GeoLite2-City.mmdb")
                     (string-append target "/GeoLite2-City.mmdb")))))))))
      (inputs
       (modify-inputs (package-inputs pkg)
         (append geolite2-country-mmdb geolite2-city-mmdb)))
      (synopsis "Caddy with Netlify DNS, Coraza WAF, MaxMind geoblocking, and Pocket ID authentication support"))))

(define caddy-security-netlify-crowdsec-coraza-maxmind
  (let ((pkg (caddy-custom
              "2.10.2"
              '("github.com/greenpau/caddy-security"
                "github.com/caddy-dns/netlify"
                ;; The next three modules are for
                ;; caddy-crowdsec-bounder; the latter two are for
                ;; optional features
                "github.com/hslatman/caddy-crowdsec-bouncer/http"
                "github.com/hslatman/caddy-crowdsec-bouncer/layer4"
                "github.com/hslatman/caddy-crowdsec-bouncer/appsec"
                "github.com/corazawaf/coraza-caddy/v2"
                "github.com/porech/caddy-maxmind-geolocation")
              "0ds06c0hbwf6lpimvsp39v6b9dxifzikv29qij8zlq35f4jkxf6s"
              "0x386sv2cw8fvix693zwqcz8d9ajrfm4gj8g932bn29c94j5dg92")))
    (package/inherit pkg
      (name "caddy-security-netlify-crowdsec-coraza-maxmind")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'install 'link-geolite-dbs
                ;; The city database is a superset of the country
                ;; database, but we bundle both: users may choose
                ;; which database to use
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (geoip-country (assoc-ref inputs "geolite2-country-mmdb"))
                         (geoip-city (assoc-ref inputs "geolite2-city-mmdb"))
                         (target (string-append out "/var/lib/geoip")))
                    (mkdir-p target)
                    (symlink
                     (string-append geoip-country "/var/lib/geoip/GeoLite2-Country.mmdb")
                     (string-append target "/GeoLite2-Country.mmdb"))
                    (symlink
                     (string-append geoip-city "/var/lib/geoip/GeoLite2-City.mmdb")
                     (string-append target "/GeoLite2-City.mmdb")))))))))
      (inputs
       (modify-inputs (package-inputs pkg)
         (append geolite2-country-mmdb geolite2-city-mmdb)))
      (synopsis "Caddy with Netlify DNS, CrowdSec,Coraza WAF, MaxMind geoblocking, and Pocket ID authentication support"))))
