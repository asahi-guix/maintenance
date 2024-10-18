(define-module (asahi guix maintenance packages website)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix build modules)
  #:use-module (asahi guix packages installer)
  #:use-module (gnu packages guile)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages))

(define-public asahi-guix-website
  (package
    (name "asahi-guix-website")
    (version "0.0.1")
    (source #f)
    (build-system copy-build-system)
    (arguments
     (list
      #:modules '((asahi guix installer data)
                  (guix build copy-build-system)
                  (guix build utils)
                  (srfi srfi-1))
      #:phases
      (with-extensions (list guile-json-4)
        (with-imported-modules (source-module-closure
                                '((asahi guix installer data))
                                #:select? import-asahi-module?)
          #~(modify-phases %standard-phases
              (delete 'unpack)
              (replace 'install
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((installer-script (assoc-ref inputs "asahi-installer-script"))
                        (web-root (string-append #$output "/share/asahi-guix-website"))
                        (os-root (string-append #$output "/share/asahi-guix-website/os")))
                    ;; Add index.html.
                    (mkdir-p web-root)
                    (invoke "touch" (string-append web-root "/index.html"))
                    ;; Add installer bootstrap script.
                    (copy-file (string-append installer-script "/bin/asahi-guix-install")
                               (string-append web-root "/install"))
                    ;; Copy operating systems and installer data.
                    (mkdir-p os-root)
                    (define os-dirs
                      (search-path-as-list
                       (list "share/asahi-installer/os")
                       (map cdr inputs)))
                    (define (symlink-os dir)
                      (for-each (lambda (file)
                                  (symlink file (string-append os-root "/" (basename file))))
                                (find-files dir)))
                    (define (merge-data sources)
                      (let ((data (map read-installer-data sources)))
                        (write-installer-data
                         (reduce merge-installer-data #f data)
                         (string-append os-root "/installer_data.json"))))
                    (define (find-data-files dir)
                      (find-files dir ".json"))
                    (for-each symlink-os os-dirs)
                    (merge-data (append-map find-data-files os-dirs))))))))))
    (home-page "https://www.asahi-guix.org")
    (inputs (list asahi-installer-script
                  asahi-installer-os-base
                  asahi-installer-os-edge
                  ;; asahi-installer-os-gnome
                  ;; asahi-installer-os-plasma
                  ;; asahi-installer-os-sway
                  ))
    (synopsis "Asahi Guix website")
    (description "This package provides the Asahi Guix website.")
    (license license:gpl3+)))
