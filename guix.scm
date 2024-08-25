(use-modules
 ((guix licenses) #:prefix license:)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages package-management)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages)
 (guix build-system gnu)
 (guix download)
 (guix gexp)
 (guix git-download)
 (guix packages)
 (guix utils)
 (srfi srfi-1))

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
