(define-module (krisb services containers)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services containers)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services containers)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (oci-pod-configuration
            oci-pod-configuration?
            oci-pod-configuration-name
            oci-pod-configuration-ports
            oci-pod-configuration-hostname
            oci-pod-configuration-network
            oci-pod-configuration-labels
            oci-pod-configuration-extra-arguments
            
            oci-pod-extension
            oci-pod-extension?
            oci-pod-extension-pods
            oci-pod-extension-containers
            oci-pod-extension-networks
            oci-pod-extension-volumes
            
            oci-container-configuration/pod
            
            podman-pods-configuration->shepherd-services
            podman-pods-service-type
            home-podman-pods-service-type))

(define-maybe/no-serialization string)

(define oci-configuration-home-service?
  (@@ (gnu services containers) oci-configuration-home-service?))

(define-configuration/no-serialization oci-pod-extension
  (pods
   (list '())
   "The list of @code{oci-pod-configuration} records representing the
pods to add.")
  (containers
   (list '())
   "The list of @code{oci-container-configuration} records representing
containers to add.")
  (networks
   (list '())
   "The list of @code{oci-network-configuration} records representing
networks to add.")
  (volumes
   (list '())
   "The list of @code{oci-volume-configuration} records representing
volumes to add."))

(define-configuration/no-serialization oci-pod-configuration
  (name
   (string)
   "The name of the Podman pod to provision.")
  (ports
   (list '())
   "Set the port or port ranges to expose from the pod. List of pairs or strings.")
  (hostname
   (maybe-string)
   "Set the hostname for the pod.")
  (network
   (maybe-string)
   "Set an OCI network for the pod.")
  (labels
   (list '())
   "The list of labels that will be used to tag the pod.")
  (extra-arguments
   (list '())
   "A list of strings, gexps or file-like objects passed to podman pod create."))

(define (oci-sanitize-mixed-list value delimiter)
  (map
   (lambda (el)
     (cond ((string? el) el)
           ((pair? el)
            (match el
              (((? string? key) . (? string? value))
               #~(string-append #$key #$delimiter #$value))
              (_ el)))
           (else el)))
   value))

(define (oci-pod-configuration->options config)
  (let ((hostname (oci-pod-configuration-hostname config))
        (network (oci-pod-configuration-network config))
        (ports (oci-sanitize-mixed-list 
                (oci-pod-configuration-ports config) ":"))
        (labels (oci-sanitize-mixed-list
                 (oci-pod-configuration-labels config) "=")))
    (apply append
           (filter (compose not unspecified?)
                   (list (if (maybe-value-set? hostname)
                             `("--hostname" ,hostname)
                             '())
                         (if (maybe-value-set? network)
                             `("--network" ,network)
                             '())
                         (append-map
                          (lambda (spec)
                            (list "-p" spec))
                          ports)
                         (append-map
                          (lambda (spec)
                            (list "--label" spec))
                          labels))))))

(define (oci-pod-extension-merge a b)
  (let ((merged-oci (oci-extension-merge
                     (oci-pod-extension->oci-extension a)
                     (oci-pod-extension->oci-extension b))))
    (oci-pod-extension
     (pods (oci-objects-merge-lst
            (oci-pod-extension-pods a)
            (oci-pod-extension-pods b)
            "pod"
            oci-pod-configuration-name))
     (containers (oci-extension-containers merged-oci))
     (networks (oci-extension-networks merged-oci))
     (volumes (oci-extension-volumes merged-oci)))))

(define (oci-pod-create-invocation runtime-cli name options extra-arguments)
  #~(list #$runtime-cli "pod" "create"
          "--name" #$name
          #$@options
          #$@extra-arguments))

(define* (oci-pods-shepherd-service runtime runtime-cli pods user group
                                    #:key verbose? home-service?)
  (define invocations
    (map
     (lambda (pod)
       (oci-pod-create-invocation
        runtime-cli
        (oci-pod-configuration-name pod)
        (oci-pod-configuration->options pod)
        (oci-pod-configuration-extra-arguments pod)))
     pods))
  
  (define entrypoint
    (with-imported-modules (source-module-closure
                            '((guix build utils)))
      (program-file
          "podman-pods-create.scm"
        #~(begin
            (use-modules (guix build utils)
                         (ice-9 format))
            
            (define (create-pod invocation verbose?)
              (when verbose?
                (format #t "Running: ~a~%" (string-join invocation " ")))
              (unless (zero? (apply system* invocation))
                (format (current-error-port) "Failed to create pod~%")))
            
            (for-each
             (lambda (inv)
               (create-pod inv #$verbose?))
             (list #$@invocations))))))
  
  (define runtime-requirement
    (if home-service?
        '()
        (if (eq? runtime 'podman)
            '(cgroups2-fs-owner cgroups2-limits
                                rootless-podman-shared-root-fs user-processes)
            '(dockerd user-processes))))
  
  (shepherd-service
    (provision '(podman-pods))
    (requirement runtime-requirement)
    (one-shot? #t)
    (documentation "Podman pod provisioning service")
    (start
     #~(lambda _
         (fork+exec-command
          (list #$entrypoint)
          #$@(if user (list #:user user) '())
          #$@(if group (list #:group group) '())
          #:environment-variables
          #$(if user
                #~(list (string-append "HOME=" 
                                       (passwd:dir (getpwnam #$user))))
                #~'()))))
    (actions
     (list
      (shepherd-action
        (name 'command-line)
        (documentation "Print the pod creation commands.")
        (procedure
         #~(lambda _
             (format #t "Pod creation commands:~%")
             (for-each
              (lambda (inv)
                (format #t "  ~a~%" (string-join inv " ")))
              (list #$@invocations)))))))))

(define (oci-container-configuration/pod container-config pod-name)
  (oci-container-configuration
    (inherit container-config)
    (requirement
     (append (oci-container-configuration-requirement container-config)
             '(podman-pods)))
    (extra-arguments
     (append (list "--pod" pod-name)
             (oci-container-configuration-extra-arguments container-config)))))

(define* (podman-pods-extension->shepherd-services ext config
                                                   #:key verbose?)
  (define runtime (oci-configuration-runtime config))
  (define home-service? (oci-configuration-home-service? config))
  (define runtime-cli
    (if home-service?
        (oci-runtime-home-cli config)
        (oci-runtime-system-cli config)))
  (define user
    (if home-service?
        #f
        (oci-configuration-user config)))
  (define group
    (if home-service?
        #f
        (if (eq? runtime 'podman)
            #~(group:name
               (getgrgid
                (passwd:gid
                 (getpwnam #$user))))
            (oci-configuration-group config))))
  
  (define pods (oci-pod-extension-pods ext))
  
  (when (and (not (null? pods))
             (not (eq? runtime 'podman)))
    (raise
     (formatted-message
      (G_ "Pods are only supported with the 'podman runtime"))))
  
  (if (null? pods)
      '()
      (list
       (oci-pods-shepherd-service
        runtime runtime-cli pods user group
        #:verbose? verbose?
        #:home-service? (oci-configuration-home-service? config)))))

(define (oci-pod-extension->oci-extension ext)
  (oci-extension
   (containers (oci-pod-extension-containers ext))
   (networks (oci-pod-extension-networks ext))
   (volumes (oci-pod-extension-volumes ext))))

(define* (podman-pods-configuration->shepherd-services config
                                                       #:key verbose?)
  (define runtime (oci-configuration-runtime config))
  (define home-service? (oci-configuration-home-service? config))
  (define runtime-cli
    (if home-service?
        (oci-runtime-home-cli config)
        (oci-runtime-system-cli config)))
  (define user
    (if home-service?
        #f
        (oci-configuration-user config)))
  (define group
    (if home-service?
        #f
        (if (eq? runtime 'podman)
            #~(group:name
               (getgrgid
                (passwd:gid
                 (getpwnam #$user))))
            (oci-configuration-group config))))
  
  (define pods (oci-configuration-pods config))
  
  (when (and (not (null? pods))
             (not (eq? runtime 'podman)))
    (raise
     (formatted-message
      (G_ "Pods are only supported with the 'podman runtime"))))
  
  (if (null? pods)
      '()
      (list
       (oci-pods-shepherd-service
        runtime runtime-cli pods user group
        #:verbose? verbose?
        #:home-service? (oci-configuration-home-service? config)))))

(define podman-pods-service-type
  (service-type
    (name 'podman-pods)
    (extensions
     (list
      (service-extension oci-service-type
                         oci-pod-extension->oci-extension)
      (service-extension shepherd-root-service-type
                         (lambda (ext)
                           (let ((config (oci-configuration (runtime 'podman))))
                             (podman-pods-extension->shepherd-services
                              ext config #:verbose? #f))))))
    (compose (lambda (args)
               (fold oci-pod-extension-merge
                     (oci-pod-extension)
                     args)))
    (extend (lambda (ext1 ext2)
              (oci-pod-extension
               (pods (append (oci-pod-extension-pods ext1)
                             (oci-pod-extension-pods ext2)))
               (containers (append (oci-pod-extension-containers ext1)
                                   (oci-pod-extension-containers ext2)))
               (networks (append (oci-pod-extension-networks ext1)
                                 (oci-pod-extension-networks ext2)))
               (volumes (append (oci-pod-extension-volumes ext1)
                                (oci-pod-extension-volumes ext2))))))
    (default-value (oci-pod-extension))
    (description
     "This service provisions Podman pods and their associated containers.")))

(define home-podman-pods-service-type
  (service-type
    (name 'home-podman-pods)
    (extensions
     (list
      (service-extension home-oci-service-type
                         oci-pod-extension->oci-extension)
      (service-extension home-shepherd-service-type
                         (lambda (ext)
                           (let ((config (for-home (oci-configuration (runtime 'podman)))))
                             (podman-pods-extension->shepherd-services
                              ext config #:verbose? #f))))))
    (compose (lambda (args)
               (fold oci-pod-extension-merge
                     (oci-pod-extension)
                     args)))
    (extend (lambda (ext1 ext2)
              (oci-pod-extension
               (pods (append (oci-pod-extension-pods ext1)
                             (oci-pod-extension-pods ext2)))
               (containers (append (oci-pod-extension-containers ext1)
                                   (oci-pod-extension-containers ext2)))
               (networks (append (oci-pod-extension-networks ext1)
                                 (oci-pod-extension-networks ext2)))
               (volumes (append (oci-pod-extension-volumes ext1)
                                (oci-pod-extension-volumes ext2))))))
    (default-value (oci-pod-extension))
    (description
     "This service provisions Podman pods and their associated containers in a Guix Home context.")))
