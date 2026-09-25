;; In previous versions of netbird, the netbird status and netbird serve commands
;; sometimes required for the netbird socket in two different locations:
;;     /var/run/netbird.sock (default, https://docs.netbird.io/client/grpc-socket)
;;     /var/run/netbird/sock (old default)
;;
;; This required specifying the socket location when using either
;; commands.  The easiest solution was to use the default socket
;; location for `netbird status` and specify the socket location in
;; the `netbird serve` command invoked by `netbird-service-type`,
;; similar to how netbird system services are for systemd, openrc,
;; sysvinit, etc.
;;
;; Additionally the `netbird serve` command does not always respect
;; the specified socket:
;; https://github.com/birdieing/netbird/issues/4269.  As of 0.75.0
;; this appears to be fixed. If this issue arises, either specify a
;; different socket in the service definitions (see comments) and/or
;; specify the correct socket with the `netbird status` command.

(define-module (krisb services netbird)
  #:use-module (krisb packages netbird)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:export (netbird-configuration
            netbird-service-type))

;; NOTE(KrisB): Module modified from
;; https://codeberg.org/technicalrenaissance/renaissance

(define-record-type* <netbird-configuration>
  netbird-configuration make-netbird-configuration
  netbird-configuration?
  (netbird netbird-configuration-netbird ; file-like
           (default netbird))
  (log-level netbird-configuration-log-level ; string
             (default "info"))
  (log-file netbird-configuration-log-file ; string
            (default "/var/log/netbird/client.log"))
  (daemon-addr netbird-configuration-daemon-addr ; string
               (default "unix:///var/run/netbird.sock"))
  (config-file netbird-configuration-config-file ; string or #f
               (default #f))
  (respawn? netbird-configuration-respawn? ; boolean
            (default #t))
  (shepherd-requirement netbird-configuration-shepherd-requirement ; list of symbols
                        (default '()))
  (extra-options netbird-configuration-extra-options ; list of strings
                 (default '())))

(define (netbird-shepherd-service config)
  (let ((netbird (netbird-configuration-netbird config))
        (log-level (netbird-configuration-log-level config))
        (log-file (netbird-configuration-log-file config))
        (daemon-addr (netbird-configuration-daemon-addr config))
        (config-file (netbird-configuration-config-file config))
        (extra (netbird-configuration-extra-options config)))
    (list (shepherd-service
            (documentation "Run the NetBird daemon.")
            (provision '(netbird-client))
            (requirement (append '(networking)
                                 (netbird-configuration-shepherd-requirement config)))
            (start #~(make-forkexec-constructor
                      (list
                       #$(file-append netbird "/bin/netbird")
                       "service" "run"
                       "--log-level" #$log-level
                       ;; Although --log-file supports paths +
                       ;; console, we use `make-forkexec-constructor`
                       ;; to take advantage of Shepherd's
                       ;; 'log-rotation' service
                       "--log-file" "console"
                       "--daemon-addr" #$daemon-addr
                       #$@(if config-file
                              (list "--config" config-file)
                              '())
                       #$@extra)
                      #:log-file #$log-file))
            (respawn? (netbird-configuration-respawn? config))
            (stop #~(make-kill-destructor))))))

(define netbird-service-type
  (service-type
    (name 'netbird)
    (extensions (list (service-extension shepherd-root-service-type
                                         netbird-shepherd-service)))
    (default-value (netbird-configuration))
    (description "Run and connect to netbird")))
