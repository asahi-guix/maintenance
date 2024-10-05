(define-module (asahi guix maintenance build nightly)
  #:use-module (asahi guix build installer)
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

(define %installer-package-dir
  "share/asahi-installer/os")

(define %output-dir
  "/tmp/asahi/guix/nightly")

(define-record-type* <website-builder>
  website-builder
  make-website-builder
  website-builder?
  (output-dir website-builder-output-dir (default %output-dir))
  (packages website-builder-packages (default #f))
  (store-path website-builder-store-path (default (%store-directory))))

(define-record-type* <website-package>
  website-package
  make-website-package
  website-package?
  (build-time website-package-build-time)
  (derivation website-package-derivation)
  (log-file website-package-log-file))

(define (installer-package-derivations? filename)
  (string-match ".*-asahi-guix-(.+)-installer-package-(.+)\\.drv" (basename filename)))

(define (find-installer-package-derivation-paths store-path)
  (map (lambda (path)
         (string-append store-path "/" path))
       (scandir store-path installer-package-derivations?)))

(define (find-installer-package-derivations store-path)
  (map read-derivation-from-file (find-installer-package-derivation-paths store-path)))

(define (derivation-output-exists? derivation)
  (let ((output (assoc-ref (derivation-outputs derivation) "out")))
    (directory-exists? (derivation-output-path output))))

(define (derivation-build-time derivation)
  (let ((output (assoc-ref (derivation-outputs derivation) "out")))
    (when (directory-exists? (derivation-output-path output))
      (stat:ctime (stat (derivation-output-path output))))))

(define (compare-build-time package-1 package-2)
  (< (website-package-build-time package-1)
     (website-package-build-time package-2)))

(define (find-packages builder store)
  (let ((store-path (website-builder-store-path builder)))
    (sort (map (lambda (derivation)
                 (website-package
                  (build-time (derivation-build-time derivation))
                  (derivation derivation)
                  (log-file (log-file store (derivation-file-name derivation)))))
               (filter derivation-output-exists?
                       (find-installer-package-derivations store-path)))
          compare-build-time)))

(define (build-website builder)
  (with-store %store
    (website-builder
     (inherit builder)
     (packages (find-packages builder %store)))))

;; (define my-derivations
;;   (find-installer-package-derivations (%store-directory)))

;; (define (installer-data-file-basename path)
;;   (let ((parts (string-split path #\/)))
;;     (when (> (length parts) 3)
;;       (list-ref parts 3))))

;; (define (installer-package-dir directory)
;;   (string-append directory "/" %installer-package-dir))

;; (define (installer-package-dir? directory)
;;   (directory-exists? (installer-package-dir directory)))

;; (define (find-installer-package-paths store-path)
;;   (map (lambda (path)
;;          (string-append store-path "/" path))
;;        (scandir store-path
;;                 (lambda (path)
;;                   (let ((directory (string-append store-path "/" path)))
;;                     (installer-package-dir? directory))))))

;; (define (resolve-package-path directory)
;;   (map (lambda (file)
;;          (cons file (read-installer-data file)))
;;        (find-files (installer-package-dir directory)
;;                    (lambda (path stats)
;;                      (string-suffix? ".json" path)))))

;; (define* (deploy-entry entry output-dir)
;;   (let ((file (car entry))
;;         (data (cdr entry)))
;;     (format #t "- ~a\n" file)))

;; (define (asahi-guix-nightly-run
;;          #:key
;;          (output-dir %output-dir)
;;          (store-path (%store-directory)))
;;   (let* ((paths (find-installer-package-paths store-path))
;;          (resolved-paths (append-map resolve-package-path paths)))
;;     (mkdir-p output-dir)
;;     (for-each (lambda (entry)
;;                 (deploy-entry entry output-dir))
;;               resolved-paths)
;;     resolved-paths))

;; (installer-data-file-basename "/gnu/store/62f3f761iw87gcdby07sjnhxsawb8rhf-asahi-guix-base-installer-package-0.0.1/share/asahi-installer/os/asahi-guix-base-1.4.0-25.e85f52e.json")

;; Getopt

(define option-spec
  '((help (single-char #\h) (value #f))
    (output-dir (single-char #\o) (value #t))
    (store-path (single-char #\s) (value #t))))

(define (store-path-option options)
  (option-ref options 'store-path (%store-directory)))

(define (output-dir-option options)
  (option-ref options 'output-dir %output-dir))

(define (show-usage)
  (format #t  "Usage: asahi-guix-nightly [options]\n\n")
  (format #t  "Options:\n")
  (format #t  "  -h, --help               Show help\n")
  (format #t  "  -o, --output-dir=DIR     Output directory (default: ~a)\n" %output-dir)
  (format #t  "  -s, --store-path=PATH    The path to the Guix store (default: ~a)\n" (%store-directory)))

(define* (asahi-guix-nightly-main args)
  (let* ((options (getopt-long args option-spec))
         (disk-images (option-ref options '() #f)))
    (if (option-ref options 'help #f)
        (show-usage)
        (build-website
         (website-builder
          (output-dir (output-dir-option options))
          (store-path (store-path-option options)))))))

(asahi-guix-nightly-main '("nightly"))

;; (define my-paths (asahi-guix-nightly-run))

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
