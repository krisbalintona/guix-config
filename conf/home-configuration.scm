;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (conf home-configuration)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services shells)
  #:use-module (krisb packages jujutsu))

(define krisb-home-environment
  (home-environment
    ;; Below is the list of packages that will show up in your
    ;; Home profile, under ~/.guix-home/profile.
    (packages (specifications->packages
	       (list
		;; Basic stuff
		"coreutils"
		"findutils"
		"grep"
		"sed"
		"gawk"
		"less"
		"which"
		"gzip"
		"bzip2"
		"xz"
		"tar"
		"diffutils"
		"file"
		"psmisc"
		"procps"
		"inetutils"
		"net-tools"
		"openssh"
		"wget"
		"curl"
		"tree"
		;; Other
		"git"
		"neovim"
		"emacs-guix"
		"emacs-master"
		;; "jujutsu"
		"jujutsu-bin")))

    ;; Below is the list of Home services.  To search for available
    ;; services, run 'guix home search KEYWORD' in a terminal.
    (services
     (append (list
	      (service home-fish-service-type
		       (home-fish-configuration
			(environment-variables '(("DISPLAY" . ":0")))))
	      (service home-bash-service-type
                       (home-bash-configuration
			(aliases '(("grep" . "grep --color=auto")
				   ("ll" . "ls -l")
				   ("ls" . "ls -p --color=auto")))
			(bashrc (list (local-file "files/.bashrc" "bashrc")))
			(bash-profile (list (local-file
					     "files/.bash_profile"
					     "bash_profile"))))))
             %base-home-services))))

krisb-home-environment
