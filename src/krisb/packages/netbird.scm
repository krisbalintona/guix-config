(define-module (krisb packages netbird)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages version-control) ; git-minimal
  #:use-module (gnu packages certs)           ; nss-certs
  #:use-module (gnu packages vpn)             ; wireguard-tools
  #:use-module (gnu packages nss)
  #:use-module (gnu packages linux)
  #:export (netbird))

;; NOTE(KrisB): Package definition copied from
;; https://codeberg.org/technicalrenaissance/renaissance/

(define %netbird-version "0.79.0")

(define %netbird-source
  (origin
    (method git-fetch)
    (uri (git-reference
           (url "https://github.com/netbirdio/netbird")
           (commit (string-append "v" %netbird-version))))
    (file-name (git-file-name "netbird" %netbird-version))
    (sha256
     (base32 "0g54jxi1x7mf77l2zm28jv3dh9alz3yw904ga53g2ahm3yx3fbq6"))))

;; Fixed-output derivation populating the Go module cache. Passing
;; #:hash / #:hash-algo through computed-file's #:options makes this a
;; genuine FOD, which the daemon grants network access. computed-file
;; returns a file-like object, so #$ ungexps it cleanly
;; (gexp->derivation returns a monadic value and cannot be ungexped).
(define (netbird-go-modules version source)
  (computed-file
      (string-append "netbird-go-modules-" version)
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (setenv "PATH"
                  (string-append #$(file-append go-1.26 "/bin") ":"
                                 #$(file-append git-minimal "/bin") ":"
                                 (getenv "PATH")))
          ;; TLS trust store, or Go's HTTPS fetch fails x509 in-sandbox.
          (setenv "SSL_CERT_FILE"
                  (string-append #$nss-certs
                                 "/etc/ssl/certs/ca-certificates.crt"))
          (setenv "SSL_CERT_DIR"
                  (string-append #$nss-certs "/etc/ssl/certs"))
          (let ((cache (string-append (getcwd) "/gomodcache")))
            (setenv "GOPATH"      (string-append (getcwd) "/go"))
            (setenv "GOMODCACHE"  cache)
            (setenv "GOCACHE"     (string-append (getcwd) "/gocache"))
            (setenv "GOSUMDB"     "off")
            (setenv "GOFLAGS"     "-mod=mod")
            (setenv "GOTELEMETRY" "off")
            (setenv "GOTOOLCHAIN" "local")

            (copy-recursively #$source "src")
            ;; Store checkout is read-only; go mod download rewrites go.sum,
            ;; so make the src tree writable first or it dies with EACCES.
            (for-each (lambda (f) (chmod f #o644))
                      (find-files "src" #:directories? #f))
            (for-each (lambda (d) (chmod d #o755))
                      (find-files "src" #:directories? #t))

            (with-directory-excursion "src"
              (invoke "go" "mod" "download" "all"))

            ;; go mod download writes the cache read-only — files AND their
            ;; parent dirs. delete-file needs write permission on the parent
            ;; directory, so make the whole cache tree writable before
            ;; stripping lock/sumdb files, else cleanup EACCESes.
            (for-each (lambda (d) (chmod d #o755))
                      (find-files cache #:directories? #t))
            (for-each (lambda (f) (chmod f #o644))
                      (find-files cache #:directories? #f))

            ;; Strip nondeterministic bits before the tree is hashed.
            (when (file-exists? (string-append cache "/cache/lock"))
              (delete-file (string-append cache "/cache/lock")))
            (when (file-exists? (string-append cache "/cache/download/sumdb"))
              (delete-file-recursively
               (string-append cache "/cache/download/sumdb")))
            (for-each delete-file (find-files cache "\\.lock$"))

            (copy-recursively cache #$output))))
    #:local-build? #f
    #:options
    ;; fakeHash: build once with zeros, read the reported "actual" base32
    ;; from the mismatch, paste it here, rebuild. #:hash wants raw bytes —
    ;; base32 returns those; never wrap in (sha256 …), which is a hasher.
    (list #:hash-algo 'sha256
          #:hash (base32 "1ipvb40kq3x7y901l0i5rcxx84r0w8j468nba2a3sjnqdar2gm3f")
          #:recursive? #t)))

(define netbird
  (package
    (name "netbird")
    (version %netbird-version)
    (source %netbird-source)
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.26
      #:import-path "github.com/netbirdio/netbird"
      #:install-source? #f
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-module-cache
            (lambda _
              ;; Anchor the cache at an absolute, always-writable path so
              ;; both this phase and 'build agree on its location regardless
              ;; of cwd. /tmp is the writable parent of the build dir; no
              ;; ".." arithmetic to get wrong.
              (let ((cache "/tmp/gomodcache"))
                (mkdir-p cache)
                (copy-recursively
                 #$(netbird-go-modules %netbird-version %netbird-source)
                 cache)
                ;; Cache copied from the FOD is read-only; make writable.
                (for-each (lambda (d) (chmod d #o755))
                          (find-files cache #:directories? #t))
                (for-each (lambda (f) (chmod f #o644))
                          (find-files cache #:directories? #f))
                ;; Force MODULE mode — go-build-system defaults to
                ;; GO111MODULE=off (GOPATH mode), which ignores the cache
                ;; and looks for unpacked sources under GOPATH/src.
                (setenv "GO111MODULE" "on")
                (setenv "GOMODCACHE" cache)
                (setenv "GOFLAGS" "-mod=mod")
                (setenv "GOSUMDB" "off")
                (setenv "GOPROXY" "off")   ; cache is complete; forbid network
                (setenv "GOTOOLCHAIN" "local")
                ;; go.sum lives in the module root, not the build root.
                (let ((gosum "src/github.com/netbirdio/netbird/go.sum"))
                  (when (file-exists? gosum) (chmod gosum #o644))))))
          (replace 'build
            (lambda _
              (with-directory-excursion
                  "src/github.com/netbirdio/netbird"
                (for-each
                 (lambda (pair)
                   (let ((pkg (car pair))
                         (bin (cdr pair)))
                     (format #t "building ~a~%" bin)
                     (invoke "go" "build"
                             "-ldflags"
                             (string-append
                              "-X github.com/netbirdio/netbird/version.version=v"
                              #$version " -s -w")
                             "-o" (string-append #$output "/bin/" bin)
                             (string-append
                              "github.com/netbirdio/netbird/" pkg))))
                 '(("client"     . "netbird")
                   ("signal"     . "netbird-signal")
                   ("management" . "netbird-management")
                   ("relay"      . "netbird-relay"))))))
          (delete 'install)
          (delete 'check))))
    (inputs (list wireguard-tools))
    (native-inputs (list git-minimal))
    (home-page "https://netbird.io")
    (synopsis "WireGuard-based mesh networking")
    (description
     "NetBird combines WireGuard with a control plane for zero-config
peer-to-peer mesh networking.")
    (license (list license:bsd-3 license:agpl3))))
