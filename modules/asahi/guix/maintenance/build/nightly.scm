(define-module (asahi guix maintenance build nightly)
  #:use-module (asahi guix build installer)
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
  #:use-module (srfi srfi-1))

(define %build-time-format
  "%Y-%m-%dT%H:%M:%S")

(define %installer-os-dir
  "share/asahi-installer/os")

(define %output-dir
  "/tmp/asahi/guix/nightly")

(define-record-type* <website-builder>
  website-builder
  make-website-builder
  website-builder?
  (max-packages website-builder-max-packages (default #f))
  (output-dir website-builder-output-dir (default %output-dir))
  (packages website-builder-packages (default #f))
  (store-path website-builder-store-path (default (%store-directory))))

(define-record-type* <website-package>
  website-package
  make-website-package
  website-package?
  (build-time website-package-build-time)
  (derivation website-package-derivation)
  (installer-data website-package-installer-data)
  (log-file website-package-log-file))

(define (installer-package-derivation-path? filename)
  (string-match ".*-asahi-guix-(.+)-installer-package-(.+)\\.drv" (basename filename)))

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
    (reduce merge-installer-data #f (map read-installer-data files))))

(define (compare-build-time package-1 package-2)
  (> (website-package-build-time package-1)
     (website-package-build-time package-2)))

(define (find-packages builder store)
  (let ((store-path (website-builder-store-path builder)))
    (sort (map (lambda (derivation)
                 (website-package
                  (build-time (derivation-build-time derivation))
                  (derivation derivation)
                  (installer-data (derivation-installer-data derivation))
                  (log-file (log-file store (derivation-file-name derivation)))))
               (filter derivation-output-path-exists?
                       (find-installer-package-derivations store-path)))
          compare-build-time)))

(define (installer-os-target-dir builder package)
  (let ((derivation (website-package-derivation package)))
    (string-append (website-builder-output-dir builder) "/os/"
                   (bytevector->base16-string (derivation-hash derivation)))))

(define (installer-os-target-path builder package source)
  (string-append (installer-os-target-dir builder package) "/" (basename source)))

(define (deploy-installer-os builder package os)
  (let* ((derivation (website-package-derivation package))
         (time (strftime %build-time-format (localtime (derivation-build-time derivation))))
         (new-os-name (string-append (installer-os-name os) " - " time)))
    (format #t "Deploying ~a ...\n" new-os-name)
    (let ((derivation (website-package-derivation package)))
      (for-each (lambda (source)
                  (let ((target (installer-os-target-path builder package source)))
                    (format #t "  ~a\n" target)
                    (mkdir-p (dirname target))
                    (symlink source target)))
                (find-files (derivation-installer-os-dir derivation)))
      (installer-os
       (inherit os)
       (name new-os-name)))))

;; (begin (deploy-website my-builder) #f)

(define (deploy-installer-data builder package)
  (let ((data (website-package-installer-data package)))
    (installer-data
     (inherit data)
     (os-list (map (lambda (os)
                     (deploy-installer-os builder package os))
                   (installer-data-os-list data))))))

(define (deploy-package builder package)
  (website-package
   (inherit package)
   (installer-data (deploy-installer-data builder package))))

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
    (deploy-packages builder)))

(define (build-website builder)
  (with-store %store
    (let ((builder (website-builder
                    (inherit builder)
                    (packages (find-packages builder %store)))))
      (deploy-website builder)
      builder)))

;; Getopt

(define option-spec
  '((help (single-char #\h) (value #f))
    (max-packages (single-char #\m) (value #t))
    (output-dir (single-char #\o) (value #t))
    (store-path (single-char #\s) (value #t))))

(define (max-packages-option options)
  (let ((max (option-ref options 'max-packages #f)))
    (and (string? max) (string->number max))))

(define (store-path-option options)
  (option-ref options 'store-path (%store-directory)))

(define (output-dir-option options)
  (option-ref options 'output-dir %output-dir))

(define (show-usage)
  (format #t  "Usage: asahi-guix-nightly [options]\n\n")
  (format #t  "Options:\n")
  (format #t  "  -h, --help                 Show help\n")
  (format #t  "  -m, --max-packages=NUM     The maximum number of packages\n")
  (format #t  "  -o, --output-dir=DIR       Output directory (default: ~a)\n" %output-dir)
  (format #t  "  -s, --store-path=PATH      The path to the Guix store (default: ~a)\n" (%store-directory)))

(define* (asahi-guix-nightly-main args)
  (let ((options (getopt-long args option-spec)))
    (if (option-ref options 'help #f)
        (show-usage)
        (build-website
         (website-builder
          (max-packages (max-packages-option options))
          (output-dir (output-dir-option options))
          (store-path (store-path-option options)))))))

;; (define my-builder (asahi-guix-nightly-main '("nightly" "-m" "x")))

;; (define my-paths (find-installer-package-derivation-paths "/gnu/store"))

;; (with-store %store
;;   (map (lambda (file)
;;          (log-file %store file))
;;        (find-installer-package-derivations "/gnu/store")))

;; (file-exists? (my-derivation-log-file my-derivation-file))

;; (with-store %store
;;   (log-file %store "/gnu/store/0ysz9bkx86w7bq8788hfrpvmav9savlw-asahi-guix-base-installer-package-0.0.1"))

;; (define my-derivation-file
;;   "/gnu/store/yn044izaxmaqb9yw62wak36br511r66w-asahi-guix-base-installer-package-0.0.1.drv")

;; (installer-package-derivations? "/gnu/store/yn044izaxmaqb9yw62wak36br511r66w-asahi-guix-base-installer-package-0.0.1.drv")
;; (find-installer-package-derivations "/gnu/store")

;; (derivation-outputs (read-derivation-from-file my-derivation-file))
