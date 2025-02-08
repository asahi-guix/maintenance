(define-module (asahi guix maintenance services bees)
  #:use-module (gnu packages base)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages linux)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (%bees-service
            bees-configuration
            bees-configuration-package
            bees-configuration-roots
            bees-configuration-thread-count
            bees-configuration-thread-factor
            bees-configuration-thread-min
            bees-configuration-verbose
            bees-service-type))

(define-record-type* <bees-configuration>
  bees-configuration make-bees-configuration bees-configuration?
  (package bees-configuration-package (default bees))
  (roots bees-configuration-roots (default '()))
  (thread-count bees-configuration-thread-count (default #f))
  (thread-factor bees-configuration-thread-factor (default 1))
  (thread-min bees-configuration-thread-min (default 0))
  (verbose bees-configuration-roots (default 8)))

(define bees-profile
  (match-lambda
    (($ <bees-configuration> package roots thread-count thread-factor thread-min verbose)
     (list coreutils util-linux package))))

(define bees-shepherd-root
  (match-lambda
    (($ <bees-configuration> package roots thread-count thread-factor thread-min verbose)
     (map (lambda (root)
            (let ((provision-name (string->symbol (format #f "bees-~a" root))))
              (shepherd-service
               (documentation (format #f "Bees ~a" root))
               (provision (list provision-name))
               (requirement '(file-systems))
               (start #~(lambda _
                          (use-modules (guix build utils))
                          (let ((log-file (string-append "/var/log/bees/" #$root ".log")))
                            (mkdir-p (dirname log-file))
                            (catch 'system-error
                              (lambda () (umount (string-append "/run/bees/mnt/" #$root)))
                              (const #f))
                            (make-forkexec-constructor
                             (list #$(file-append package "/sbin/beesd")
                                   #$@(if (number? thread-count)
                                          `(,(string-append "--thread-count="
                                                            (number->string thread-count)))
                                          '())
                                   (string-append "--thread-factor=" #$(number->string thread-factor))
                                   (string-append "--thread-min=" #$(number->string thread-min))
                                   (string-append "--verbose=" #$(number->string verbose))
                                   #$root)
                             #:environment-variables
                             (list (string-append "PATH=" #$(file-append util-linux "/sbin")))
                             #:log-file log-file))))
               (stop #~(make-kill-destructor)))))
          roots))))

(define bees-service-type
  (service-type
   (name 'bees)
   (extensions
    (list (service-extension
           profile-service-type
           bees-profile)
          (service-extension
           shepherd-root-service-type
           bees-shepherd-root)))
   (default-value (bees-configuration))
   (description "Run bees, the deduplication agent for btrfs file systems.")))

(define %bees-service
  (service bees-service-type
           (bees-configuration
            (roots '("4e136cfb-d433-46df-9255-f05afcd0dcfc"))
            (verbose 5))))
