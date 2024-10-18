(define-module (asahi guix maintenance build website)
  #:use-module (asahi guix installer data)
  #:use-module (asahi guix installer os)
  #:use-module (asahi guix installer script)
  #:use-module (guix base16)
  #:use-module (guix build utils)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-1)
  #:export (build-website
            make-website-builder
            make-website-package
            website-builder
            website-builder-domain
            website-builder-max-packages
            website-builder-output-dir
            website-builder-packages
            website-builder-repo-base
            website-builder-report-tag
            website-builder-report-url
            website-builder-script-path
            website-builder-store-path
            website-builder?
            website-package
            website-package-build-time
            website-package-installer-data
            website-package-log-file
            website-package?))

(define %domain
  "www.asahi-guix.org")

(define %report-tag
  "agx-prod")

(define %repo-base
  (format #f "https://~a/builds" %domain))

(define %report-url
  "https://stats.asahi-guix.org/report")

(define %build-time-format
  "%Y-%m-%d %H:%M:%S")

(define %installer-os-dir
  "share/asahi-installer/os")

(define %output-dir
  "/tmp/asahi-guix/website/builds")

(define-record-type* <website-builder>
  website-builder
  make-website-builder
  website-builder?
  (domain website-builder-domain (default %domain))
  (max-packages website-builder-max-packages (default #f))
  (output-dir website-builder-output-dir (default %output-dir))
  (packages website-builder-packages (default #f))
  (repo-base website-builder-repo-base (default %repo-base))
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
  (installer-data website-package-installer-data)
  (log-file website-package-log-file))

(define (website-package-compare-build-time package-1 package-2)
  (> (website-package-build-time package-1) (website-package-build-time package-2)))

(define (website-builder-installer-data-url builder)
  (string-append "https://" (website-builder-domain builder) "/builds/" %installer-data-filename))

(define (website-builder-installer-data builder)
  (let ((packages (website-builder-packages builder)))
    (reduce merge-installer-data #f (map website-package-installer-data packages))))

(define (file-extension filename)
  (let ((matches (string-match ".*\\.(.+)" (basename filename))))
    (when (regexp-match? matches)
      (match:substring matches 1))))

(define (installer-package-derivation-path? filename)
  (string-match ".*-asahi-installer-os-(.+)\\.drv" (basename filename)))

(define (find-installer-package-derivation-paths store-path)
  (map (lambda (path)
         (string-append store-path "/" path))
       (scandir store-path installer-package-derivation-path?)))

(define (find-installer-package-derivations store-path)
  (filter derivation?
          (map (lambda (file)
                 (with-exception-handler
                     (lambda (exception)
                       (format #t "Warning: Can't read derivation: ~a.\n" file)
                       #f)
                   (lambda () (read-derivation-from-file file))
                   #:unwind? #t))
               (find-installer-package-derivation-paths store-path))))

(define (derivation-output-path-exists? derivation)
  (directory-exists? (derivation->output-path derivation)))

(define (derivation-installer-os-dir derivation)
  (string-append (derivation->output-path derivation) "/" %installer-os-dir))

(define (derivation-installer-os-dir-exists? derivation)
  (directory-exists? (derivation-installer-os-dir derivation)))

(define (derivation-build-time derivation)
  (when (derivation-output-path-exists? derivation)
    (stat:ctime (stat (derivation->output-path derivation)))))

(define (derivation-installer-data-files derivation)
  (when (derivation-output-path-exists? derivation)
    (find-files (derivation-installer-os-dir derivation)
                (lambda (path stats)
                  (string-suffix? ".json" path)))))

(define (derivation-installer-data derivation)
  (let ((files (derivation-installer-data-files derivation)))
    (reduce merge-installer-data #f
            (remove null? (map (lambda (file)
                                 (with-exception-handler
                                     (lambda (e)
                                       (format #t "Warning: Invalid installer metadata ~a.\n" file)
                                       #f)
                                   (lambda ()
                                     (installer-data-apply-package
                                      (read-installer-data file)
                                      (lambda (package)
                                        (string-append (dirname file) "/" package))))
                                   #:unwind? #t))
                               files)))))

(define (find-packages builder store)
  (let ((store-path (website-builder-store-path builder)))
    (sort (filter website-package?
                  (map (lambda (derivation)
                         (let ((log-file (log-file store (derivation-file-name derivation)))
                               (metadata (derivation-installer-data derivation)))
                           (when (installer-data? metadata)
                             (website-package
                              (build-time (derivation-build-time derivation))
                              (derivation derivation)
                              (installer-data metadata)
                              (log-file log-file)))))
                       (filter derivation-installer-os-dir-exists?
                               (find-installer-package-derivations store-path))))
          website-package-compare-build-time)))

(define (installer-os-derivation-name package)
  (let ((derivation (website-package-derivation package)))
    (string-drop-right (basename (derivation-file-name derivation)) 4)))

(define (installer-os-new-name builder package os)
  (let* ((derivation (website-package-derivation package))
         (time (localtime (derivation-build-time derivation))))
    (string-append (installer-os-name os) " ("  (strftime %build-time-format time) ")")))

;; Deploy

(define (deploy-metadata-source-path os)
  (string-replace-substring (installer-os-package os) ".zip" ".json"))

(define (deploy-metadata-target-path builder package)
  (string-append "os/" (installer-os-derivation-name package) ".json"))

(define (deploy-package-target-path builder package)
  (string-append "os/" (installer-os-derivation-name package) ".zip"))

(define (deploy-installer-os builder package os)
  (with-directory-excursion (website-builder-output-dir builder)
    (let ((new-os-name (installer-os-new-name builder package os)))
      (let ((derivation (website-package-derivation package))
            (package (deploy-package-target-path builder package))
            (metadata (deploy-metadata-target-path builder package)))
        (mkdir-p (dirname package))
        (when (file-exists? package)
          (delete-file package))
        (symlink (installer-os-package os) package)
        (mkdir-p (dirname metadata))
        (when (file-exists? metadata)
          (delete-file metadata))
        (symlink (deploy-metadata-source-path os) metadata)
        (installer-os
         (inherit os)
         (name new-os-name)
         (package package))))))

(define (deploy-package-installer-data builder package)
  (let ((data (website-package-installer-data package)))
    (installer-data
     (inherit data)
     (os-list (map (lambda (os)
                     (deploy-installer-os builder package os))
                   (installer-data-os-list data))))))

(define (deploy-log-file-path builder package)
  (let ((log-file (website-package-log-file package)))
    (when (and (string? log-file) (file-exists? log-file))
      (string-append "os/" (installer-os-derivation-name package)
                     (string-append ".log." (file-extension log-file))))))

(define (deploy-log-file builder package)
  (with-directory-excursion (website-builder-output-dir builder)
    (let ((log-file (website-package-log-file package)))
      (if (and (string? log-file) (file-exists? log-file))
          (let ((target (deploy-log-file-path builder package)))
            (mkdir-p (dirname target))
            (when (file-exists? target)
              (delete-file target))
            (symlink log-file target)
            target)
          (format #t "Warning: No log file found for ~a.\n"
                  (derivation-name (website-package-derivation package)))))))

(define (deploy-package builder package)
  (website-package
   (inherit package)
   (installer-data (deploy-package-installer-data builder package))
   (log-file (deploy-log-file builder package))))

(define (deploy-website-installer-data builder)
  (with-directory-excursion (website-builder-output-dir builder)
    (let ((data (website-builder-installer-data builder)))
      (when (installer-data? data)
        (write-installer-data
         (installer-data
          (os-list (map (lambda (os)
                          (installer-os
                           (inherit os)
                           (package (basename (installer-os-package os)))))
                        (installer-data-os-list data))))
         %installer-data-filename))
      builder)))

(define (website-builder-installer-script-target builder)
  (string-append (website-builder-output-dir builder) "/install"))

(define (deploy-website-installer-script builder)
  (let ((source (website-builder-script-path builder)))
    (when (and (string? source) (file-exists? source))
      (write-installer-script
       (installer-script
        (inherit (read-installer-script source))
        (installer-data (website-builder-installer-data-url builder))
        (installer-data-alt (website-builder-installer-data-url builder))
        (repo-base (website-builder-repo-base builder))
        (report (website-builder-report-url builder))
        (report-tag (website-builder-report-tag builder)))
       (website-builder-installer-script-target builder)))
    builder))

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
     (deploy-website-installer-data
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
  (let* ((data (website-package-installer-data package))
         (os (car (installer-data-os-list data)))
         (log-file (website-package-log-file package)))
    `(li ,(installer-os-name os)
         " - "
         (a (@ (href ,(installer-os-package os)))
            "installer package")
         " : "
         (a (@ (href ,(installer-os-metadata os)))
            "installer metadata")
         ,@(if (string? log-file)
               `(" : " (a (@ (href ,(website-package-log-file package)))
                          "build logs"))
               '()))))

(define (website-sxml-installer-list packages)
  `(ul (@ (id "installer-list"))
       ,@(map website-sxml-installer-list-item packages)))

(define (website-sxml-body builder)
  (let ((packages (website-builder-packages builder)))
    `(body
      (div (@ (style "font-family: 'Open Sans', sans-serif;"))
           (h1 (@ (style "font-family: 'Montserrat', sans-serif")) "Asahi Guix builds")
           (p "These are automated builds that have not been tested, use at your own risk.")
           (p (pre (@ (style "font-size: 20px"))
                   ,(format #f "curl ~a/install | sh" (website-builder-repo-base builder))))
           ,(website-sxml-installer-list packages)))))

(define (website-sxml builder)
  `(html ,(website-sxml-head)
         ,(website-sxml-body builder)))

(define (website-index-file builder)
  (string-append (website-builder-output-dir builder) "/index.html"))

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
  (format #t  "Usage: asahi-guix-builds [options]\n\n")
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

;; (define my-builder (asahi-website-builder-main '("asahi-build-website" "-s" "/gnu/store/3k6gfmkwyxh7g7dpaf71c7lci4xsy6j4-asahi-installer-script-0.0.1/bin/asahi-guix-installer.sh")))
