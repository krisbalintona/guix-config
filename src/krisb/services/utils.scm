(define-module (krisb services utils)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1)
  #:export (direct-symlink-configuration
            direct-symlink-service-type))

(define (gexp-or-string? x)
  (or (gexp? x) (string? x)))

(define-configuration/no-serialization direct-symlink-configuration
  (link-path gexp-or-string "String or gexp evaluating to the symlink path.")
  (target-path gexp-or-string "String or gexp evaluating to the target path of symlink."))

(define (direct-symlink-activation configs)
  "Return a gexp creating the symlinks described by the list CONFIGS."
  #~(begin
      #$@(map (lambda (config)
                #~(let ((link #$(direct-symlink-configuration-link-path config))
                        (target #$(direct-symlink-configuration-target-path config)))
                    (mkdir-p (dirname link))
                    (false-if-exception (delete-file link))
                    (symlink target link)))
              configs)))

(define direct-symlink-service-type
  (service-type
    (name 'direct-symlink)
    (extensions (list (service-extension home-activation-service-type
                                         direct-symlink-activation)))
    (compose concatenate)
    (extend append)
    (default-value '())
    (description "Create direct symlinks from LINK-PATH to TARGET-PATH.")))
