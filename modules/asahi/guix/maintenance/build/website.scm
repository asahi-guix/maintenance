(define-module (asahi guix maintenance build website)
  #:use-module (asahi guix build installer metadata)
  #:use-module (asahi guix build installer os)
  #:use-module (guix base16)
  #:use-module (guix build utils)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-1))

(define %domain
  "www.asahi-guix.org")

(define %installer-metadata-filename
  "installer-data.json")

(define %report-tag
  "agx-prod")

(define %report-url
  "https://stats.asahi-guix.org/report")

(define %build-time-format
  "%Y-%m-%d %H:%M:%S")

(define %installer-os-dir
  "share/asahi-installer/os")

(define %output-dir
  "/tmp/asahi-guix/website")

(define-record-type* <website-builder>
  website-builder
  make-website-builder
  website-builder?
  (domain website-builder-domain (default %domain))
  (installer-metadata-filename website-builder-installer-metadata-filename (default %installer-metadata-filename))
  (max-packages website-builder-max-packages (default #f))
  (output-dir website-builder-output-dir (default %output-dir))
  (packages website-builder-packages (default #f))
  (report-tag website-builder-report-tag (default %report-tag))
  (report-url website-builder-report-url (default %report-url))
  (script-path website-builder-script-path (default #f))
  (store-path website-builder-store-path (default (%store-directory))))

(define-record-type* <website-package>
  website-package
  make-website-package
  website-package?
  (build-time website-package-build-time)
  (derivation website-package-derivation)
  (installer-metadata website-package-installer-metadata)
  (log-file website-package-log-file))

(define (website-builder-installer-data-url builder)
  (string-append "https://" (website-builder-domain builder) "/"
                 (website-builder-installer-metadata-filename builder)))

(define (website-builder-metadata-path builder)
  (string-append (website-builder-output-dir builder) "/"
                 (website-builder-installer-metadata-filename builder)))

(define (website-builder-installer-metadata builder)
  (let ((packages (website-builder-packages builder)))
    (reduce merge-installer-metadata #f (map website-package-installer-metadata packages))))

(define (website-package-name package)
  (let ((data (website-package-installer-metadata package)))
    (when (installer-metadata? data)
      (let ((os (car (installer-metadata-os-list data))))
        (when (installer-os? os)
          (installer-os-package os))))))

(define (file-extension filename)
  (let ((matches (string-match ".*\\.(.+)" (basename filename))))
    (when (regexp-match? matches)
      (match:substring matches 1))))

(define (installer-package-derivation-path? filename)
  (string-match ".*-asahi-installer-package-(.+)\\.drv" (basename filename)))

(define (find-installer-package-derivation-paths store-path)
  (map (lambda (path)
         (string-append store-path "/" path))
       (scandir store-path installer-package-derivation-path?)))

(define (find-installer-package-derivations store-path)
  (map read-derivation-from-file (find-installer-package-derivation-paths store-path)))

(define (derivation-output-path-exists? derivation)
  (directory-exists? (derivation->output-path derivation)))

(define (derivation-installer-os-dir derivation)
  (string-append (derivation->output-path derivation) "/" %installer-os-dir))

(define (derivation-installer-os-dir-exists? derivation)
  (directory-exists? (derivation-installer-os-dir derivation)))

(define (derivation-build-time derivation)
  (when (derivation-output-path-exists? derivation)
    (stat:ctime (stat (derivation->output-path derivation)))))

(define (derivation-installer-metadata-files derivation)
  (when (derivation-output-path-exists? derivation)
    (find-files (derivation-installer-os-dir derivation)
                (lambda (path stats)
                  (string-suffix? ".json" path)))))

(define (derivation-installer-metadata derivation)
  (let ((files (derivation-installer-metadata-files derivation)))
    (reduce merge-installer-metadata #f
            (remove null? (map (lambda (file)
                                 (with-exception-handler
                                     (lambda (e)
                                       (format #t "Warning: Invalid installer metadata ~a.\n" file)
                                       #f)
                                   (lambda () (read-installer-metadata file))
                                   #:unwind? #t))
                               files)))))

(define (compare-build-time package-1 package-2)
  (> (website-package-build-time package-1)
     (website-package-build-time package-2)))

(define (find-packages builder store)
  (let ((store-path (website-builder-store-path builder)))
    (sort (filter website-package?
                  (map (lambda (derivation)
                         (let ((log-file (log-file store (derivation-file-name derivation)))
                               (metadata (derivation-installer-metadata derivation)))
                           (when (installer-metadata? metadata)
                             (website-package
                              (build-time (derivation-build-time derivation))
                              (derivation derivation)
                              (installer-metadata metadata)
                              (log-file log-file)))))
                       (filter derivation-installer-os-dir-exists?
                               (find-installer-package-derivations store-path))))
          compare-build-time)))

(define (installer-os-relative-dir package)
  (let ((derivation (website-package-derivation package)))
    (string-append "os/" (string-drop-right (basename (derivation-file-name derivation)) 4))))

(define (installer-os-target-dir builder package)
  (let ((derivation (website-package-derivation package)))
    (string-append (website-builder-output-dir builder) "/"
                   (installer-os-relative-dir package))))

(define (installer-os-target-path builder package source)
  (string-append (installer-os-target-dir builder package) "/" (basename source)))

(define (installer-os-new-package package os)
  (string-append (installer-os-relative-dir package) "/" (installer-os-package os)))

(define (installer-os-new-name builder package os)
  (let* ((derivation (website-package-derivation package))
         (time (localtime (derivation-build-time derivation))))
    (string-append (installer-os-name os) " ("  (strftime %build-time-format time) ")")))

;; Deploy

(define (deploy-installer-os builder package os)
  (let ((new-os-name (installer-os-new-name builder package os)))
    ;; (format #t "Deploying ~a ...\n" new-os-name)
    (let ((derivation (website-package-derivation package)))
      (for-each (lambda (source)
                  (let ((target (installer-os-target-path builder package source)))
                    ;; (format #t "  ~a\n" target)
                    (mkdir-p (dirname target))
                    (symlink source target)))
                (find-files (derivation-installer-os-dir derivation)))
      (installer-os
       (inherit os)
       (name new-os-name)
       (package (installer-os-new-package package os))))))

(define (deploy-package-installer-metadata builder package)
  (let ((data (website-package-installer-metadata package)))
    (installer-metadata
     (inherit data)
     (os-list (map (lambda (os)
                     (deploy-installer-os builder package os))
                   (installer-metadata-os-list data))))))

(define (deploy-log-file builder package)
  (if (website-package-log-file package)
      (let* ((source (website-package-log-file package))
             (extension (file-extension source))
             (log-file (string-replace-substring
                        (website-package-name package)
                        ".zip" (string-append ".log." extension)))
             (target (string-append
                      (installer-os-target-dir builder package)
                      "/" log-file)))
        (mkdir-p (dirname target))
        (symlink source target)
        (string-append (installer-os-relative-dir package) "/" log-file))
      (format #t "Warning: No log file found for ~a.\n"
              (derivation-name (website-package-derivation package)))))

(define (deploy-package builder package)
  (website-package
   (inherit package)
   (installer-metadata (deploy-package-installer-metadata builder package))
   (log-file (deploy-log-file builder package))))

(define (deploy-website-installer-metadata builder)
  (let ((target (website-builder-metadata-path builder))
        (data (website-builder-installer-metadata builder)))
    (when (installer-metadata? data)
      (mkdir-p (dirname target))
      (write-installer-metadata data target))
    builder))

(define (website-builder-installer-script-target builder)
  (string-append (website-builder-output-dir builder) "/install.sh"))

(define (deploy-website-installer-script builder)
  (let ((source (website-builder-script-path builder)))
    (when (and (string? source) (file-exists? source))
      (let ((target (website-builder-installer-script-target builder)))
        (mkdir-p (dirname target))
        (copy-file source target)
        (chmod target #o755)
        (substitute* target
          (("INSTALLER_DATA=.*")
           (string-append
            "INSTALLER_DATA="
            (website-builder-installer-data-url builder) "\n"))
          (("INSTALLER_DATA_ALT=.*")
           (string-append
            "INSTALLER_DATA_ALT="
            (website-builder-installer-data-url builder) "\n"))
          (("REPORT=.*")
           (string-append
            "REPORT="
            (website-builder-report-url builder) "\n"))
          (("REPORT_TAG=.*")
           (string-append
            "REPORT_TAG="
            (website-builder-report-tag builder) "\n")))
        builder))))

(define (deploy-packages builder)
  (website-builder
   (inherit builder)
   (packages (map (lambda (package)
                    (deploy-package builder package))
                  (website-builder-packages builder)))))

(define (deploy-website builder)
  (let ((output-dir (website-builder-output-dir builder)))
    (when (directory-exists? output-dir)
      (delete-file-recursively output-dir))
    (mkdir-p output-dir)
    (deploy-website-installer-script
     (deploy-website-installer-metadata
      (deploy-packages builder)))))

;; Render

(define (website-sxml-head)
  `(head (title "Asahi Guix builds")
         (style "
a:link {
  text-decoration: none;
}

a:visited {
  text-decoration: none;
}

a:hover {
  text-decoration: none;
}

a:active {
  text-decoration: none;
}")))

(define (installer-os-build-logs os)
  (string-replace-substring (installer-os-package os) ".zip" ".log"))

(define (installer-os-metadata os)
  (string-replace-substring (installer-os-package os) ".zip" ".json"))

(define (website-sxml-installer-list-item package)
  (let* ((data (website-package-installer-metadata package))
         (os (car (installer-metadata-os-list data))))
    `(li ,(installer-os-name os)
         " - "
         (a (@ (href ,(installer-os-package os)))
            "installer package")
         " : "
         (a (@ (href ,(installer-os-metadata os)))
            "installer metadata")
         " : "
         (a (@ (href ,(website-package-log-file package)))
            "build logs"))))

(define (website-sxml-installer-list packages)
  `(ul (@ (id "installer-list"))
       ,@(map website-sxml-installer-list-item packages)))

(define (website-sxml-body builder)
  (let ((packages (website-builder-packages builder)))
    `(body
      (div (@ (style "font-family: 'Open Sans', sans-serif;"))
           (h1 (@ (style "font-family: 'Montserrat', sans-serif")) "Asahi Guix builds")
           (p "These are automated builds that have not been tested, use at your own risk.")
           (p (pre (@ (style "font-size: 20px")) "curl https://www.asahi-guix.org/builds | sh"))
           ,(website-sxml-installer-list packages)))))

(define (website-sxml builder)
  `(html ,(website-sxml-head)
         ,(website-sxml-body builder)))

(define (website-index-file builder)
  (string-append (website-builder-output-dir builder) "/builds.html"))

(define (render-website builder)
  (let ((file (website-index-file builder)))
    (mkdir-p (dirname file))
    (call-with-output-file file
      (lambda (port)
        (set-port-encoding! port "UTF-8")
        (sxml->xml (website-sxml builder) port)))
    builder))

(define (build-website builder)
  (with-store %store
    (let ((builder (website-builder
                    (inherit builder)
                    (packages (find-packages builder %store)))))
      (render-website (deploy-website builder)))))

;; Getopt

(define option-spec
  '((help (single-char #\h) (value #f))
    (max-packages (single-char #\m) (value #t))
    (output-dir (single-char #\o) (value #t))
    (script-path (single-char #\s) (value #t))
    (store-path (single-char #\S) (value #t))))

(define (max-packages-option options)
  (let ((max (option-ref options 'max-packages #f)))
    (and (string? max) (string->number max))))

(define (script-path-option options)
  (option-ref options 'script-path #f))

(define (store-path-option options)
  (option-ref options 'store-path (%store-directory)))

(define (output-dir-option options)
  (option-ref options 'output-dir %output-dir))

(define (show-usage)
  (format #t  "Usage: asahi-guix-nightly [options]\n\n")
  (format #t  "Options:\n")
  (format #t  "  -S, --store-path=PATH      The path to the Guix store (default: ~a)\n" (%store-directory))
  (format #t  "  -h, --help                 Show help\n")
  (format #t  "  -m, --max-packages=NUM     The maximum number of packages\n")
  (format #t  "  -o, --output-dir=DIR       Output directory (default: ~a)\n" %output-dir)
  (format #t  "  -s, --script-path=PATH     The path to the Asahi installer script\n"))

(define* (asahi-website-builder-main args)
  (let ((options (getopt-long args option-spec)))
    (if (option-ref options 'help #f)
        (show-usage)
        (build-website
         (website-builder
          (max-packages (max-packages-option options))
          (output-dir (output-dir-option options))
          (script-path (script-path-option options))
          (store-path (store-path-option options)))))))

;; (define my-builder (asahi-website-builder-main '("asahi-build-website" "-s" "/gnu/store/sifb7aj7nfc55rg6id9w2divcr7gi5vp-asahi-installer-script-0.0.1/bin/asahi-guix-installer.sh")))
