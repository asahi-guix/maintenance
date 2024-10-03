(define-module (asahi guix maintenance build nightly)
  #:use-module (asahi guix build installer)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1))

(define %installer-package-dir
  "share/asahi-installer/os")

(define %output-dir
  "/tmp/asahi/guix/nightly")

(define (installer-package-dir directory)
  (string-append directory "/" %installer-package-dir))

(define (installer-package-dir? directory)
  (directory-exists? (installer-package-dir directory)))

(define (find-installer-package-paths store-path)
  (map (lambda (path)
         (string-append store-path "/" path))
       (scandir store-path
                (lambda (path)
                  (let ((directory (string-append store-path "/" path)))
                    (installer-package-dir? directory))))))

(define (resolve-package-path directory)
  (map (lambda (file)
         (cons file (read-installer-data file)))
       (find-files (installer-package-dir directory)
                     (lambda (path stats)
                       (string-suffix? ".json" path)))))

(define* (asahi-guix-nightly-run
          #:key
          (output-dir %output-dir)
          (store-path (%store-directory)))
  (let* ((paths (find-installer-package-paths store-path))
         (resolved-paths (append-map resolve-package-path paths)))
    (mkdir-p output-dir)
    (for-each (lambda (entry)
                (let ((dir (car entry))
                      (data (cdr entry)))
                  (format #t "- ~a\n" dir)))
              resolved-paths)))

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
  (display "Usage: asahi-guix-nightly [options]\n\n")
  (display "Options:\n")
  (display "  -h, --help                      Show help\n")
  (display "  -o, --output-dir=DIR            Output directory\n")
  (display "  -s, --store-path=PATH           The path to the Guix store\n"))

(define* (asahi-guix-nightly-main args)
  (let* ((options (getopt-long args option-spec))
         (disk-images (option-ref options '() #f)))
    (if (or (option-ref options 'help #f)
            (null? args))
        (show-usage)
        (asahi-guix-nightly-run
         #:output-dir (output-dir-option options)
         #:store-path (store-path-option options)))))

;; (define my-data (make-asahi-installer-package (list "/gnu/store/hfr97d38hpgq2skh10192f1ik1smvrx7-asahi-base-image")))
;; (asahi-guix-nightly-main '("nightly"))

;; (define my-paths
;;   (asahi-guix-nightly-run))
