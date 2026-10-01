(define-module (krisb config machines features substitute-servers)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (guix gexp)
  #:export (feature-substitute-servers))

(define %signing-keys-dir
  ;; The cwd should be the repository root
  (string-append (getcwd) "/signing-keys"))

(define (signing-keys-path path)
  (string-append %signing-keys-dir "/" path))

(define (feature-substitute-servers)
  (list
   ;; Nonguix
   (simple-service 'substitutes-nonguix
       guix-service-type
     (guix-extension
       (authorized-keys
        (list (local-file (signing-keys-path "nonguix.pub"))))
       (substitute-urls
        (list "https://substitutes.nonguix.org"))))
   ;; Guix Moe
   (simple-service 'substitutes-guix-moe
       guix-service-type
     (guix-extension
       (authorized-keys
        (list (local-file (signing-keys-path "guix-moe.pub"))))
       (substitute-urls (list "https://cache-cdn.guix.moe"))))))
