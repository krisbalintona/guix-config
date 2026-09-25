(define-module (krisb services envsubst)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages bash)      ; For bash
  #:use-module (gnu packages gettext)   ; For gettext-minimal
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-35)
  #:export (envsubst-substitution
            envsubst-substitution?
            envsubst-substitution-fields
            envsubst-substitution-name
            envsubst-substitution-template
            envsubst-substitution-output
            envsubst-substitution-environment-file
            envsubst-substitution-user
            envsubst-substitution-group
            envsubst-substitution-permissions
            envsubst-substitution-requireemnt
            envsubst-substitution-documentation

            envsubst-service-configuration
            envsubst-service-configuration?
            envsubst-service-configuration-envsubst
            envsubst-service-configuration-bash
            envsubst-service-configuration-substitutions
            envsubst-service-configuration-shepherd-requirement

            substitutions->envsubst-service-configuration
            envsubst-substitution->shepherd-service
            envsubst-service-type))


;;;
;;; Configuration records
;;;

(define (string-or-file-like? value)
  (if (or (string? value)
          (file-like? value)
          (gexp? value))
      value
      (error "expected a string, gexp, or file-like object, but got:" value)))

(define (string-or-gexp? value)
  (if (or (string? value)
          (gexp? value))
      value
      (error "expected a string or gexp, but got:" value)))

(define (list-of-envsubst-substitutions? value)
  (if (list-of envsubst-substitution?)
      value
      (error "expected a list of envsubst-substitution record objects, but got:" value)))

(define-configuration/no-serialization envsubst-substitution
  (name
   (symbol)
   "A unique symbol naming this substitution.  It is used to derive the
generated Shepherd service's provision, i.e.,
@code{envsubst-@var{name}}.")
  (template
   (string-or-file-like)
   "The @command{envsubst} template: a string (path), a gexp, or a
file-like object.")
  (output
   (string-or-gexp)
   "The absolute path as a string, or a gexp evaluating to one, where the
substituted file will be written.")
  (environment-file
   (string-or-gexp)
   "A string, or a gexp evaluating to one, giving the absolute path of a
file that will be sourced before running @command{envsubst}, to load
the environment variables referenced in @var{template}.  This file
should be in dotenv format, such as the what is produced by
@code{sops-secret->secret-file} with @code{#:output-type} set to
@code{\"dotenv\"}.")
  (user
   (string "root")
   "The group user of the output file.")
  (group
   (string "root")
   "The group owner of the output file.")
  (permissions
   (number #o600)                       ; Read and write to owner only
   "The Unix permissions given to @var{output} once it has been created.")
  (requirement
   (list-of-symbols '())
   "Extra Shepherd requirements for this substitution's service, on top of
the parent service's @code{shepherd-requirement} field.")
  (documentation
   (string "Create a file substituting environment variables in a template.")
   "The generated Shepherd service's documentation string."))

(define-configuration/no-serialization envsubst-service-configuration
  (envsubst
   (package gettext-minimal)
   "The package providing the @command{envsubst} binary.")
  (bash
   (package bash)
   "The package used to source each substitution's
@code{environment-file}.")
  (substitutions
   (list-of-envsubst-substitutions '())
   "The list of @code{envsubst-substitution} records managed by
@code{envsubst-service-type}.  One Shepherd service is created per
entry.")
  (shepherd-requirement
   (list-of-symbols '())
   "List of Shepherd services that must be started before any substitution
is performed."))


;;;
;;; Shepherd service
;;;

(define* (envsubst-substitution->shepherd-service-name substitution #:key home-service?)
  (symbol-append (if home-service? 'home-envsubst- 'envsubst-)
                 (envsubst-substitution-name substitution)))

(define* (envsubst-substitution->shepherd-service substitution config #:key home-service?)
  (define envsubst
    (envsubst-service-configuration-envsubst config))
  (define bash*
    (envsubst-service-configuration-bash config))

  ;; FIXME 2026-09-12: Right now since this is a oneshot service, I
  ;; don't think anything is logged in the output of 'herd status'.
  ;; However, it would be useful to log what this does, such as which
  ;; files are deleted and created, and on failures, what exactly
  ;; failed.
  ;;
  ;; TODO 2026-09-24: We should report (stdout?) if there is a
  ;; difference between a file that exists at the output path and the
  ;; substitution result we've overwritten it with (fail? backup old
  ;; file?)
  (shepherd-service
    (provision
     (list (envsubst-substitution->shepherd-service-name substitution #:home-service? home-service?)))
    (requirement
     (append (envsubst-service-configuration-shepherd-requirement config)
             (envsubst-substitution-requirement substitution)))
    (documentation (envsubst-substitution-documentation substitution))
    (one-shot? #t)
    (start
     #~(lambda ()
         (let ((template #$(envsubst-substitution-template substitution))
               (dotenv #$(envsubst-substitution-environment-file substitution))
               (output #$(envsubst-substitution-output substitution)))
           (format #t "envsubst: dotenv=~a exists?=~a~%" dotenv (file-exists? dotenv))
           (format #t "envsubst: template=~a exists?=~a~%" template (file-exists? template))
           (when (file-exists? output) ; envsubst doesn't overwrite, so we delete first
             (format #t "envsubst: deleting stale ~a~%" output)
             (delete-file output))
           (mkdir-p (dirname output))
           (let* ((status
                   (system* #$(file-append bash* "/bin/bash") "-c"
                            (string-append "set -a; "
                                           "source " dotenv "; "
                                           "set +a; "
                                           #$(file-append envsubst "/bin/envsubst")
                                           " < " template " > " output)))
                  (exit-val (status:exit-val status)))
             (format #t "envsubst: exit-val=~a~%" exit-val)
             (when (zero? exit-val)
               (let ((user #$(envsubst-substitution-user substitution))
                     (group #$(envsubst-substitution-group substitution))
                     (permissions #$(envsubst-substitution-permissions substitution)))
                 (chmod output permissions)
                 (unless #$home-service?
                   (chown output
                          (passwd:uid (getpwnam user)) ; user is a string name
                          (group:gid (getgrnam group)))))) ; group is a string name
             (zero? exit-val)))))))


;;;
;;; Service-type
;;;

(define (substitutions->envsubst-service-configuration config substitutions)
  (envsubst-service-configuration
   (inherit config)
   (substitutions
    (append (envsubst-service-configuration-substitutions config)
            substitutions))))

(define (envsubst-shepherd-services config)
  (map (cut envsubst-substitution->shepherd-service <> config)
       (envsubst-service-configuration-substitutions config)))

(define envsubst-service-type
  (service-type
    (name 'envsubst)
    (extensions
     (list (service-extension shepherd-root-service-type
                              envsubst-shepherd-services)))
    (compose concatenate)
    (extend substitutions->envsubst-service-configuration)
    (default-value (envsubst-service-configuration))
    (description
     "Run @command{envsubst} against one or more templates, creating a
dedicated one-shot Shepherd service per substitution.")))
