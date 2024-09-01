(define-module (asahi guix maintenance packages guile-xyz)
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

(define %version "0.1.0")

(define development-packages
  '("autoconf" "automake" "guile-ares-rs" "gettext-minimal" "texinfo" "help2man"))

(define source-root-directory
  (dirname (dirname (dirname (dirname (dirname (current-source-directory)))))))

(define vcs-file?
  (or (git-predicate source-root-directory) (const #t)))

(define source-checkout
  (local-file source-root-directory
              "asahi-guix-maintenance-checkout"
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
             (list pkg-config guile-next guix)))
    (inputs (list guile-next guix))
    ;; (propagated-inputs (list guile-config guile-lib))
    (synopsis "Asahi Guix Maintenance")
    (description "The docs, notes and code to maintain Asahi Guix.")
    (home-page "https://github.com/asahi-guix/maintenance")
    (license license:gpl3+)))

asahi-guix-maintenance
