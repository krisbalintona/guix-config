(define-module (krisb home services audio)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages music)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:export (home-easyeffects-configuration
            home-easyeffects-configuration?
            home-easyeffects-service-type))

(define-record-type* <home-easyeffects-configuration>
  home-easyeffects-configuration make-home-easyeffects-configuration
  home-easyeffects-configuration?
  (package home-easyeffects-configuration-package ; Package
           (default easyeffects))
  (extra-packages home-easyeffects-configuration-extra-packages ; List of packages
                  ;; These external plugins provide various audio
                  ;; effects:
                  ;; - Linux Studio Plugins (lsp-plugins): equalizer,
                  ;;   compressor, delay, loudness and others
                  ;; - Calf Studio Gear (calf): limiter, exciter, bass
                  ;; enhancer and others
                  ;; - MDA LV2 (mda-lv2): bass loudness
                  ;; - x42 plugins (x42-plugins): autotune
                  ;; - Zam plugins (zam-plugins): maximizer
                  ;;
                  ;; 2026-11-12 NOTE: The DeepFilterNet LADSPA plugin
                  ;; (deep noise remover) is not packaged in Guix
                  ;; currently.
                  (default (list lsp-plugins calf mda-lv2 x42-plugins zam-plugins)))
  (arguments home-easyeffects-configuration-arguments ; List of strings
             ;; 2026-11-11 TODO: The packaged version of EasyEffects
             ;; is currently 7.2.5.  But starting version 8.0.0, this
             ;; flag has been deprecated in favor of the
             ;; --service-mode flag.  When EasyEffects is upgraded to
             ;; >8.0.0, the flag below should be be changed.
             (default (list "--gapplication-service")))
  (log-file home-easyeffects-configuration-log-file ; #f, file path, or a gexp evaluating to a file path
            (default #~(string-append (getenv "HOME") "/.local/state/easyeffects/easyeffects.log")))
  (respawn? home-easyeffects-configuration-respawn? ; boolean
            (default #t)))

(define (home-easyeffects-profile-packages config)
  (cons (home-easyeffects-configuration-package config)
        (home-easyeffects-configuration-extra-packages config)))

(define (home-easyeffects-shepherd-services config)
  (match-record config <home-easyeffects-configuration>
                (package arguments log-file respawn?)
    (list (shepherd-service
            (provision '(easyeffects))
            ;; The 'wayland-display' Shepherd service only succeeds
            ;; when there is an existing Wayland session.  And its
            ;; running value is the value of WAYLAND_DISPLAY env var
            (requirement '(wayland-display))
            (documentation "Run EasyEffects in the background.")
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
                        (cons #$(file-append package "/bin/easyeffects")
                              '#$arguments)
                        #:log-file #$log-file
                        #:environment-variables
                        ;; EasyEffects needs a env var display value
                        ;; in order to open its graphical UI.
                        (cons (string-append "WAYLAND_DISPLAY="
                                             (service-running-value (lookup-service 'wayland-display)))
                              (default-environment-variables)))))
            (stop #~(make-kill-destructor))
            (respawn? respawn?)))))

(define home-easyeffects-service-type
  (service-type
    (name 'home-easyeffects)
    (extensions
     (list (service-extension home-profile-service-type
                              home-easyeffects-profile-packages)
           (service-extension home-shepherd-service-type
                              home-easyeffects-shepherd-services)))
    (default-value (home-easyeffects-configuration))
    (description
     "Install EasyEffects and its external plugins, then run it in the
background.  For the graphical interface, the service requires the
@code{wayland-display} Shepherd service (see
@code{home-wayland-service-type}).")))
