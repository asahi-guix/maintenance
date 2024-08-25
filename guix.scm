(define-module (asahi-maintenance-package)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define vcs-file?
  (or (git-predicate (dirname (dirname (current-source-directory))))
      (const #t)))

(define-public asahi-maintenance
  (package
    (name "asahi-maintenance")
    (version "0.0.1")
    (source (local-file "." "checkout" #:recursive? #t #:select? vcs-file?))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'hall
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (invoke "hall" "build" "--execute"))))))
    (native-inputs
     (list autoconf-2.71 automake guile-3.0 guile-hall pkg-config texinfo guix))
    (inputs (list guile-3.0))
    (propagated-inputs (list guile-config guile-lib))
    (synopsis "Asahi Guix Maintenance")
    (description "Asahi Guix Maintenance")
    (home-page "https://github.com/asahi-guix/maintenance")
    (license license:gpl3+)))

asahi-maintenance
