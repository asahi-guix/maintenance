(define-module (asahi-guix-maintenance-package)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define %version "0.1.0")

(define development-packages
  '("autoconf" "automake" "guile-ares-rs" "gettext-minimal" "texinfo" "help2man" "guix"))

(define vcs-file?
  (or (git-predicate (string-append (current-source-directory) "/../.."))
      (const #t)))

(define source-checkout
  (local-file "../.." "asahi-guix-maintenance-checkout"
              #:recursive? #t
              #:select? vcs-file?))

(define-public asahi-guix-maintenance
  (package
    (name "asahi-guix-maintenance")
    (version %version)
    (source source-checkout)
    (build-system gnu-build-system)
    (native-inputs
     ;; Use 'specification->package' to get the latest version of
     ;; the development packages.
     (append (map specification->package development-packages)
             (list pkg-config guile-next)))
    (inputs (list guile-3.0))
    ;; (propagated-inputs (list guile-config guile-lib))
    (synopsis "Asahi Guix Maintenance")
    (description "Asahi Guix Maintenance")
    (home-page "https://github.com/asahi-guix/maintenance")
    (license license:gpl3+)))

asahi-guix-maintenance
