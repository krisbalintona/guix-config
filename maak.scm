(define-module (maak)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (maak maak))

(define entry-point
  (make-parameter "guix"))

(define src-dir
  "src")

(define load-paths
  (~ "-L ~a" src-dir))

;; NOTE: These options make builds faster at the expensive of disk
;; usage (/tmp/) CPU usage
(define fast-build-args
  "-c 0 -M 3")

(define accepted-hosts
  (list "mute" "sublation"))

(define wsl-host?
  ;; Linux distributions inside WSL and WSL2 have "WSL" in their OS
  ;; release name
  (string-contains (utsname:release (uname)) "WSL"))

(define hostname
  (let ((host (gethostname)))
    (if (member host accepted-hosts) host
        (error "Current host is not supported!"))))

(define env-dir
  "env")

(define channels-file
  (in-vicinity env-dir "channels.scm"))

(define channels-lock-file
  (in-vicinity env-dir
               (string-append "channels-lock-" hostname ".scm")))

(define machines-path-components
  (let* ((components '("krisb" "config" "machines"))
         (path (string-join components "/"
                            'suffix)))
    (if (file-exists? path)
        (error (~
                "Issue with Maak file: path to machines' configuration files does not exist (\"~\")"
                path)) components)))

(define (host-config-dir)
  (string-join (append (list src-dir) machines-path-components
                       (list hostname)) "/"
               'suffix))

(define (get-config-expression hostname type)
  (let ((config-dir (host-config-dir)))
    (if (file-exists? config-dir)
        (match type
          ('system (if (file-exists? (in-vicinity config-dir "system.scm"))
                       (~ "(@ (~a ~a system) ~a-operating-system)"
                          (string-join machines-path-components) hostname
                          hostname)
                       (error (~ "No system configuration for host \"~a\""
                                 hostname))))
          ('home (if (file-exists? (in-vicinity config-dir "home.scm"))
                     (~ "(@ (~a ~a home) ~a-home-environment)"
                        (string-join machines-path-components) hostname
                        hostname)
                     (error (~ "No home configuration for host \"~a\""
                               hostname)))))
        (error (~ "No configuration files for host \"~a\"" hostname)))))

(define (generate-config-task level subcommand)
  (unless (memq level
                '(system home))
    (error "LEVEL must be 'system or 'home." level))
  (let ((expression (get-config-expression hostname level)))
    ($ (append (list (entry-point)
                     (symbol->string level) subcommand)
               (if wsl-host?
                   '()
                   (list fast-build-args))
               (list load-paths "--verbosity=3"
                     (~ "--expression='~a'" expression)))
       #:verbose? #t)))

(define (default)
  ($ '("maak" "--list")))


;;;
;;; Lock files
;;;

(define (lock)
  "Create a channels file for the current Guix profile."
  (let ((already-exists? (file-exists? channels-lock-file)))
    ($ (list (entry-point)
             "describe"
             "--format=channels"
             channels-file
             ">"
             channels-lock-file)
       #:verbose? #t)
    (log-info (~ "~a lock file at ~a~%"
                 (if already-exists? "Updated existing" "Created new")
                 channels-lock-file))))


;;;
;;; Pull
;;;

(define (pull)
  "Upgrade to latest version of channels."
  ($ (list (entry-point) "pull" "-C" channels-file)
     #:verbose? #t))

(define (pull-lock)
  "Upgrade or downgrade to version of channels specified by host's lock file."
  ($ (list (entry-point) "pull" "-C" channels-lock-file "--allow-downgrades")
     #:verbose? #t))

(define (upgrade)
  "Run a \"guix pull\" then create a lock file."
  (pull)
  (lock))


;;;
;;; System tasks
;;;

(define (system)
  "Reconfigure Guix system."
  (generate-config-task 'system "reconfigure"))

(define (system-build)
  "Only build the Guix system profile."
  (generate-config-task 'system "build"))

(define (system-preview)
  "Use \"guix time-machine\" to verify that a pull then system reconfigure succeeds."
  (parameterize ((entry-point (~ "guix time-machine -C ~a --" channels-file)))
    (system-build)))


;;;
;;; Home tasks
;;;

(define (home)
  "Reconfigure Guix home."
  (generate-config-task 'home "reconfigure"))

(define (home-build)
  "Only build the Guix home profile."
  (generate-config-task 'home "build"))

(define (home-preview)
  "Use \"guix time-machine\" to verify that a pull then home reconfigure succeeds."
  (parameterize ((entry-point (~ "guix time-machine -C ~a --" channels-file)))
    (home-build)))


;;;
;;; Development
;;;

(define (fmt . files)
  "Run \"guix style\" on one or more files."
  ($ `(,(entry-point) "style" "-f"
       ,@files)
     #:verbose? #t)
  (log-info (~ "Formatted: ~a~%"
               (string-join files ", "))))

(define (build . packages)
  "Build one or more Guix packages."
  ($ `(,(entry-point) "build"
       ,@packages)
     #:verbose? #t)
  (log-info (~ "Built packages: ~a~%"
               (string-join packages ", "))))


;;;
;;; Status (diagnostic information)
;;;

(define (status)
  "Report information relevant to the current host."
  (display (~ "Host                             : ~a~%" hostname))
  (display (~ "Lock file                        : ~a~%" channels-lock-file))
  (display (~ "Host's config file directory     : ~a~%"
              (host-config-dir))))
