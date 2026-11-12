(define-module (krisb home services netbird)
  #:use-module (krisb packages netbird)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services shepherd)
  #:export (home-netbird-ui-configuration
            home-netbird-ui-configuration?
            home-netbird-ui-service-type))

;;
;;; Netbird desktop UI
;;
(define-record-type* <home-netbird-ui-configuration>
  home-netbird-ui-configuration make-home-netbird-ui-configuration
  home-netbird-ui-configuration?
  (package home-netbird-ui-configuration-package ; file-like
           (default netbird-ui))
  (log-file home-netbird-ui-configuration-log-file ; #f, string (file-path), or gexp that evaluates to a string
            (default #~(string-append (getenv "HOME") "/.local/state/netbird-ui/netbird-ui.log")))
  (respawn? home-netbird-ui-configuration-respawn? ; boolean
            (default #t)))

(define (home-netbird-shepherd-service config)
  (match-record config <home-netbird-ui-configuration>
                (package log-file respawn?)
    (list (shepherd-service
            (provision '(netbird-ui))
            ;; The 'wayland-display' Shepherd service only succeeds
            ;; when there is an existing Wayland session.  And its
            ;; running value is the value of WAYLAND_DISPLAY env var
            (requirement '(wayland-display))
            ;; 2026-11-13 NOTE: We use 'fork+exec-command' inside a
            ;; 'lambda' instead of 'make-forkexec-constructor' by
            ;; itself because the value of '#:environment-variables'
            ;; needs to be evaluated at Shepherd service start time,
            ;; not when the Shepherd service is registered (which
            ;; would happen if we used procedures as the values in
            ;; 'make-forkexec-constructor').  Instead, delay
            ;; evaluation of the value of that keyword by putting it
            ;; all inside a procedure.
            ;;
            ;; This method matches when Rutherther describes here:
            ;; https://lists.gnu.org/archive/html/help-guix/2025-02/msg00063.html.
            ;;
            ;; This workaround will not be necessary in future
            ;; Shepherd releases (current Shepherd version is 1.0.9).
            ;; With https://codeberg.org/shepherd/shepherd/pulls/124,
            ;; which was merged in Aug 11, 2026, users may use
            ;; 'latent' values in '#:environment-variables' to achieve
            ;; evaluation at service start time rather than
            ;; call/registration time.
            (start #~(lambda _
                       (fork+exec-command
                        (list #$(file-append package "/bin/netbird-ui"))
                        #:log-file #$log-file
                        #:environment-variables
                        ;; The UI needs an env var display value in
                        ;; order to start.
                        (cons (string-append "WAYLAND_DISPLAY="
                                             (service-running-value (lookup-service 'wayland-display)))
                              (default-environment-variables)))))
            (stop #~(make-kill-destructor))
            (respawn? respawn?)))))

(define home-netbird-ui-service-type
  (service-type
    (name 'home-netbird-ui)
    (extensions (list (service-extension home-shepherd-service-type
                                         home-netbird-shepherd-service)))
    (default-value (home-netbird-ui-configuration))
    (description "Launch Netbird desktop UI")))
