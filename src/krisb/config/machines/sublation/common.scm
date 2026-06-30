(define-module (krisb config machines sublation common)
  #:use-module (krisb config common)
  )

(define-public %sublation-sops-age-key-file
  (let ((path "/home/krisbalintona/.config/sops/age/keys.txt"))
    (if (file-exists? path)
        path
        (error "Age key file not found:" path))))
