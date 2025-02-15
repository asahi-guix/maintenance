(define-module (asahi-guix-maintenance-package)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix channels)
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

(define asahi-guix
  (guix-for-channels
   (list (channel
          (name 'guix)
          (url "https://git.savannah.gnu.org/git/guix.git")
          (introduction
           (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
             "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
         (channel
          (name 'asahi)
          (url "https://github.com/asahi-guix/channel")
          (branch "main")
          (introduction
           (make-channel-introduction
            "3eeb493b037bea44f225c4314c5556aa25aff36c"
            (openpgp-fingerprint
             "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))))

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
             (list asahi-guix guile-git guile-gcrypt guile-ssh guile-sqlite3 pkg-config)))
    (inputs (list guile-next guile-json-4))
    (synopsis "Asahi Guix Maintenance")
    (description "The docs, notes and code to maintain Asahi Guix.")
    (home-page "https://github.com/asahi-guix/maintenance")
    (license license:gpl3+)))

asahi-guix-maintenance
