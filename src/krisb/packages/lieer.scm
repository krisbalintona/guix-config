;; NOTE 2025-05-23: The above packages are the result of trying to
;; package python-lieer, the lieer python package.  We leverage the
;; “pypi” Guix importer, which imports python package metadata from
;; https://pypi.org/ and generates a package definition that we can
;; start with.  Read about the pypi importer in this Guix manual page:
;;
;;     (guix) Invoking guix import
;;
;; Additionally, read about the pyproject-build-system in the Guix
;; manual page below:
;;
;;     (guix) Build Systems

(define-module (krisb packages lieer)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz))

;; The Guix version of this package is out of date;
;; google-api-python-client requires version 0.2.0, so we provide an
;; updated version here.
;; Initially imported via “guix import pypi google-api-python-client”
(define-public python-google-auth-httplib2
  (package
   (name "python-google-auth-httplib2")
   (version "0.2.0")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "google-auth-httplib2" version))
     (sha256
      (base32 "018fckshilc6z2qpj1pclh8haanbq3lr85w6p4g4z5wgyjnppaiq"))))
   (build-system pyproject-build-system)
   ;; FIXME 2025-05-23: Ran into some issues during the test phase
   ;; related to pip.  I disabled here; ideally we don’t do that.
   (arguments
    (list
     #:phases #~(modify-phases %standard-phases
                               (delete 'check))))
   (propagated-inputs (list python-google-auth python-httplib2
                            ;; 2025-05-23: It seems like pyparsing is
                            ;; a requirement that isn’t listed
                            python-pyparsing))
   (native-inputs (list python-setuptools python-wheel))
   (home-page
    "https://github.com/GoogleCloudPlatform/google-auth-library-python-httplib2")
   (synopsis "Google Authentication Library: httplib2 transport")
   (description "Google Authentication Library: httplib2 transport.")
   (license license:asl2.0)))

;; Initially imported via “guix import pypi google-api-python-client”
(define-public python-google-api-python-client
  (package
   (name "python-google-api-python-client")
   (version "2.170.0")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "google_api_python_client" version))
     (sha256
      (base32 "1cg2p5mg1dxa8gvxqhzdsxzj36wxqnmy051jfaiqwh8idy2s3wvm"))))
   (build-system pyproject-build-system)
   ;; FIXME 2025-05-23: Ran into some issues during the test phase
   ;; related to pip.  I disabled here; ideally we don’t do that.
   (arguments
    (list
     #:phases #~(modify-phases %standard-phases
                               (delete 'check))))
   (propagated-inputs (list python-google-api-core python-google-auth
                            ;; 2025-05-23: Guix package out of date.
                            ;; We provide an updated version above.
                            python-google-auth-httplib2
                            python-httplib2 python-uritemplate))
   (native-inputs (list python-setuptools python-wheel))
   (home-page "https://github.com/googleapis/google-api-python-client/")
   (synopsis "Google API Client Library for Python")
   (description "Google API Client Library for Python.")
   (license license:asl2.0)))

;; Initially imported via “guix import pypi lieer”
(define-public python-lieer
  (package
   (name "python-lieer")
   (version "1.6")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "lieer" version))
     (sha256
      (base32 "18ljyhyphasv0vvm7nlsadk8dp59hkrcjnnjq4y36d6h5nska308"))))
   (build-system pyproject-build-system)
   (propagated-inputs (list python-google-auth-oauthlib python-notmuch2
                            python-tqdm
                            ;; 2025-05-23: This is a hidden
                            ;; requirement.  It is not packaged in
                            ;; Guix, so we provide it ourselves
                            ;; above.
                            python-google-api-python-client))
   (native-inputs (list python-setuptools python-wheel
                        ;; 2025-05-23: This is apparently a missing
                        ;; requirement.  So I’ve added it.
                        python-pytest))
   (home-page "https://github.com/gauteh/lieer")
   (synopsis
    "Fast fetch and two-way tag synchronization between notmuch and GMail")
   (description
    "Fast fetch and two-way tag synchronization between notmuch and GMail.")
   (license license:gpl3+))) ; The imported license was missing, so we correct it here
