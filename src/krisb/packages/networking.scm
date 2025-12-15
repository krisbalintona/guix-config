(define-module (krisb packages networking)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:export (nftables-geoip))

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

(define-public nftables-geoip
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
