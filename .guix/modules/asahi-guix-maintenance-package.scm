(define-module (asahi-guix-maintenance-package)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(display "ASAHI-GUIX-MAINTENANCE-PACKAGE\n")
(format #t "CURRENT SOURCE DIR: ~a\n" (current-source-directory))

(define %version "0.1.0")

(define development-packages
  '("autoconf" "automake" "guile-ares-rs" "gettext-minimal" "texinfo" "help2man"))

(define vcs-file?
  (or (git-predicate (string-append (current-source-directory) "/../.."))
      (const #t)))

(define source-checkout
  (local-file "../.." "asahi-guix-maintenance-checkout"
              #:recursive? #t
              #:select? vcs-file?))

(format #t "SOURCE CHECKOUT: ~a\n" source-checkout)

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
             (list pkg-config guile-next (current-guix))))
    (inputs (list guile-next))
    ;; (propagated-inputs (list guile-config guile-lib))
    (synopsis "Asahi Guix Maintenance")
    (description "The docs, notes and code to maintain Asahi Guix.")
    (home-page "https://github.com/asahi-guix/maintenance")
    (license license:gpl3+)))

(format #t "PACKAGE: ~a\n" asahi-guix-maintenance)

asahi-guix-maintenance
