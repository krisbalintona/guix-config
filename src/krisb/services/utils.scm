(define-module (krisb services utils)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:export (direct-symlink-service))

(define (direct-symlink-service name link-path target-path)
  "Create a service that makes LINK-PATH a symlink pointing directly at TARGET-PATH"
  (simple-service name
      home-activation-service-type
    #~(let ((link #$link-path)
            (target #$target-path))
        (mkdir-p (dirname link))
        (false-if-exception (delete-file link))
        (symlink target link))))
