(define-module (krisb services home envsubst)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (krisb services envsubst)
  #:export (home-envsubst-service-type))

(define (home-envsubst-shepherd-services config)
  (map (cut envsubst-substitution->shepherd-service <> config  #:home-service? #t)
       (envsubst-service-configuration-substitutions config)))

(define home-envsubst-service-type
  (service-type
    (name 'home-envsubst)
    (extensions
     (list (service-extension home-shepherd-service-type
                              home-envsubst-shepherd-services)))
    (compose concatenate)
    (extend substitutions->envsubst-service-configuration)
    (default-value (envsubst-service-configuration))
    (description
     "Run @command{envsubst} against one or more templates, creating a
dedicated one-shot Home Shepherd service per substitution.")))
