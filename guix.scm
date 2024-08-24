(use-modules
 (gnu packages)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages package-management)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (guix build-system gnu)
 (guix download)
 (guix gexp)
 ((guix licenses) #:prefix license:)
 (guix packages)
 (srfi srfi-1))

(package
  (name "asahi-maintenance")
  (version "0.0.1")
  (source
   (local-file
    (dirname (current-filename))
    #:recursive?
    #t
    #:select?
    (lambda (file stat)
      (not (any (lambda (my-string)
                  (string-contains file my-string))
                (list ".git" ".dir-locals.el" "guix.scm"))))))
  (build-system gnu-build-system)
  (arguments `())
  (native-inputs
   (list autoconf automake guile-hall pkg-config texinfo guix))
  (inputs (list guile-3.0))
  (synopsis "Asahi Guix Maintenance")
  (description "Asahi Guix Maintenance")
  (home-page "https://github.com/asahi-guix/maintenance")
  (license license:gpl3+))
