(define-module (krisb packages netbird)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)             ; substitute-keyword-arguments
  #:use-module (guix gexp)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages version-control) ; git-minimal
  #:use-module (gnu packages certs)           ; nss-certs
  #:use-module (gnu packages vpn)             ; wireguard-tools
  #:use-module (gnu packages nss)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages dns)       ; openresolv
  #:use-module (gnu packages freedesktop) ; xdg-utils
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)       ; GTK 4
  #:use-module (gnu packages node)      ; node-lts
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages webkit)    ; webkitgtk
  #:use-module (gnu packages xorg)      ; libx11
  #:export (netbird
            netbird-ui))

;; NOTE(KrisB): Package definition copied from
;; https://codeberg.org/technicalrenaissance/renaissance/

(define %netbird-version "0.81.2")

(define %netbird-source
  (origin
    (method git-fetch)
    (uri (git-reference
           (url "https://github.com/netbirdio/netbird")
           (commit (string-append "v" %netbird-version))))
    (file-name (git-file-name "netbird" %netbird-version))
    (sha256
     (base32 "1c68pqs2041ay5iqm5zp0i8fylr8vm7nwziggz3zg467gc2kf54j"))))

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
    (list #:hash-algo 'sha256
          #:hash (base32 "00pgfnw8mjj2chya4k62fvbcf4rsfdqxkbd97ncg5q2gxjc43qvb")
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
          (add-after 'build 'wrap-netbird
            (lambda _
              (wrap-program (string-append #$output "/bin/netbird")
                ;; The openresolv package provides 'resolvconf', which
                ;; Netbird may sometimes use it to update the contents
                ;; of /etc/resolv.conf
                `("PATH" ":" prefix
                  (,(string-append #$(this-package-input "openresolv") "/sbin"))))))
          (delete 'install)
          (delete 'check))))
    (inputs (list wireguard-tools openresolv))
    (native-inputs (list git-minimal))
    (home-page "https://netbird.io")
    (synopsis "WireGuard-based mesh networking")
    (description
     "NetBird combines WireGuard with a control plane for zero-config
peer-to-peer mesh networking.")
    (license (list license:bsd-3 license:agpl3))))

;; netbird-ui: the desktop UI (Wails v3, GTK4 + WebKitGTK 6, with a
;; React/TypeScript frontend).  It lives in the same Go module as the
;; client, so it reuses `netbird-go-modules' for the Go side.  The
;; frontend needs two more things that cannot be produced in the
;; offline build container: node_modules and the Wails-generated
;; TypeScript bindings.  `netbird-ui-frontend-deps' fetches both in a
;; second fixed-output derivation, same technique as above.
;;
;; DRAFT, untested.  Things to check on the first build:
;; - the package manager: pnpm if the frontend has a pnpm-lock.yaml
;;   (version pinned below, match `packageManager' in
;;   client/ui/frontend/package.json), otherwise `npm ci'.

;; Matches `packageManager' in client/ui/frontend/package.json.
(define %pnpm-version "11.4.0")

(define (netbird-ui-frontend-deps version source)
  (computed-file
      (string-append "netbird-ui-frontend-deps-" version)
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let* ((top (getcwd))
                 (cacert (string-append #$nss-certs
                                        "/etc/ssl/certs/ca-certificates.crt"))
                 (pnpm-dir (string-append top "/pnpm-tool")))
            (setenv "PATH"
                    (string-append #$(file-append go-1.26 "/bin") ":"
                                   #$(file-append node-lts "/bin") ":"
                                   #$(file-append git-minimal "/bin") ":"
                                   (getenv "PATH")))
            (setenv "HOME" top)
            (setenv "CI" "true")
            ;; TLS trust store for Go (Node ships its own roots)
            (setenv "SSL_CERT_FILE" cacert)
            (setenv "SSL_CERT_DIR" (string-append #$nss-certs "/etc/ssl/certs"))
            (setenv "GOPATH"      (string-append top "/go"))
            (setenv "GOMODCACHE"  (string-append top "/gomodcache"))
            (setenv "GOCACHE"     (string-append top "/gocache"))
            (setenv "GOSUMDB"     "off")
            (setenv "GOFLAGS"     "-mod=mod")
            (setenv "GOTELEMETRY" "off")
            (setenv "GOTOOLCHAIN" "local")
            (setenv "CGO_ENABLED" "0")

            (copy-recursively #$source "src")
            (for-each (lambda (f) (chmod f #o644))
                      (find-files "src" #:directories? #f))
            (for-each (lambda (d) (chmod d #o755))
                      (find-files "src" #:directories? #t))

            (with-directory-excursion "src/client/ui/frontend"
              (if (file-exists? "pnpm-lock.yaml")
                  (begin
                    (invoke "npm" "install" "--prefix" pnpm-dir
                            (string-append "pnpm@" #$%pnpm-version))
                    (invoke "node"
                            (string-append pnpm-dir
                                           "/node_modules/pnpm/bin/pnpm.cjs")
                            "install" "--frozen-lockfile" "--ignore-scripts"
                            "--config.node-linker=hoisted"
                            "--store-dir" (string-append top "/pnpm-store")))
                  (invoke "npm" "ci" "--ignore-scripts"))
              ;; Strip anything that embeds timestamps or absolute
              ;; build paths so the output hash is stable
              (for-each (lambda (f)
                          (when (file-exists? f)
                            (delete-file-recursively f)))
                        '("node_modules/.bin" "node_modules/.cache"
                          "node_modules/.vite"))
              (for-each delete-file
                        (append
                         (find-files "node_modules"
                                     "^\\.pnpm-workspace-state.*\\.json$")
                         (find-files "node_modules" "^\\.modules\\.yaml$"))))

            ;; Take Go modules from the already-pinned
            ;; netbird-go-modules output and forbid network access, so
            ;; nothing can float.
            (let ((cache (string-append top "/gomodcache")))
              (copy-recursively #$(netbird-go-modules version source) cache)
              (for-each (lambda (d) (chmod d #o755))
                        (find-files cache #:directories? #t))
              (for-each (lambda (f) (chmod f #o644))
                        (find-files cache #:directories? #f)))
            (setenv "GOPROXY" "off")

            ;; Wails TypeScript bindings, generated from the Go
            ;; services.
            (with-directory-excursion "src/client/ui"
              (invoke "go" "run" "github.com/wailsapp/wails/v3/cmd/wails3"
                      "generate" "bindings" "-clean=true" "-ts"))

            (mkdir-p #$output)
            (copy-recursively "src/client/ui/frontend/node_modules"
                              (string-append #$output "/node_modules"))
            (copy-recursively "src/client/ui/frontend/bindings"
                              (string-append #$output "/bindings")))))
    #:local-build? #f
    #:options
    (list #:hash-algo 'sha256
          #:hash (base32 "0qr1x1fmnmd7r6acf541cj6vpb7gqk8wni3arx3al9sl3z85nwhd")
          #:recursive? #t)))

(define netbird-ui
  (package
    (inherit netbird)
    (name "netbird-ui")
    (arguments
     (substitute-keyword-arguments (package-arguments netbird)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'set-module-cache 'unpack-frontend-deps
              (lambda _
                (let ((frontend
                       "src/github.com/netbirdio/netbird/client/ui/frontend")
                      (deps #$(netbird-ui-frontend-deps %netbird-version
                                                        %netbird-source)))
                  (copy-recursively (string-append deps "/node_modules")
                                    (string-append frontend "/node_modules"))
                  (copy-recursively (string-append deps "/bindings")
                                    (string-append frontend "/bindings"))
                  (for-each (lambda (f)
                              (let ((s (lstat f)))
                                (unless (eq? (stat:type s) 'symlink)
                                  (chmod f (logior (stat:perms s) #o200)))))
                            (find-files frontend #:directories? #t)))))
            (add-before 'build 'build-frontend
              (lambda _
                (with-directory-excursion
                    "src/github.com/netbirdio/netbird/client/ui/frontend"
                  (invoke "node" "node_modules/typescript/bin/tsc")
                  (invoke "node" "node_modules/vite/bin/vite.js"
                          "build" "--mode" "production"))))
            (replace 'build
              (lambda _
                ;; GTK4/WebKitGTK bindings need cgo
                (setenv "CGO_ENABLED" "1")
                (with-directory-excursion
                    "src/github.com/netbirdio/netbird"
                  (mkdir-p (string-append #$output "/bin"))
                  ;; `production' is the Wails build tag.
                  (invoke "go" "build" "-tags" "production" "-trimpath"
                          "-ldflags"
                          (string-append
                           "-X github.com/netbirdio/netbird/version.version=v"
                           #$%netbird-version " -s -w")
                          "-o" (string-append #$output "/bin/netbird-ui")
                          "github.com/netbirdio/netbird/client/ui"))))
            ;; The client's wrapper phase wraps bin/netbird, which the
            ;; UI package does not have
            (delete 'wrap-netbird)
            (add-after 'build 'install-extras
              (lambda _
                (let* ((ui "src/github.com/netbirdio/netbird/client/ui")
                       (apps (string-append #$output "/share/applications"))
                       (icons (string-append
                               #$output
                               "/share/icons/hicolor/1024x1024/apps")))
                  (install-file (string-append ui "/build/linux/netbird-ui.desktop")
                                apps)
                  (substitute* (string-append apps "/netbird-ui.desktop")
                    (("/usr/bin/netbird-ui") "netbird-ui"))
                  (mkdir-p icons)
                  (copy-file (string-append ui "/build/appicon.png")
                             (string-append icons "/netbird-ui.png"))
                  ;; The UI shells out to xdg-open for the SSO login.
                  (wrap-program (string-append #$output "/bin/netbird-ui")
                    `("PATH" ":" prefix
                      (,(string-append #$(this-package-input "xdg-utils")
                                       "/bin")))))))))))
    (inputs (list gtk webkitgtk glib libx11 xdg-utils))
    (native-inputs (modify-inputs (package-native-inputs netbird)
                     (prepend node-lts pkg-config)))
    (synopsis "NetBird desktop UI")
    (description
     "Graphical client for NetBird, a WireGuard-based peer-to-peer
overlay network.  It talks to the @command{netbird} daemon over its
gRPC socket.")
    (license license:bsd-3)))
