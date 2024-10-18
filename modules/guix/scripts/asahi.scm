(define-module (guix scripts asahi)
  #:use-module (guix config)
  #:use-module (guix ui)
  #:autoload   (guix colors) (supports-hyperlinks? file-hyperlink)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix store)
  #:autoload   (guix base16) (bytevector->base16-string)
  #:autoload   (guix store database)
  (register-valid-path store-database-file call-with-database)
  #:autoload   (guix build store-copy) (copy-store-item)
  #:autoload   (guix describe) (current-profile)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix diagnostics)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix profiles)
  #:use-module (guix scripts)
  #:autoload   (guix channels) (channel-name
                                channel-url
                                channel-branch
                                channel-commit)
  #:use-module (guix scripts build)
  #:autoload   (guix scripts package) (delete-generations
                                       delete-matching-generations
                                       list-installed)
  #:autoload   (guix scripts pull) (channel-commit-hyperlink)
  #:autoload   (guix graph) (export-graph node-type
                                          graph-backend-name lookup-backend)
  #:use-module (guix scripts system reconfigure)
  #:use-module (guix build utils)
  #:autoload   (guix progress) (progress-reporter/bar
                                call-with-progress-reporter)
  #:use-module ((guix docker) #:select (%docker-image-max-layers))
  #:use-module (gnu build image)
  #:use-module (gnu build install)
  #:autoload   (gnu build file-systems)
  (find-partition-by-label find-partition-by-uuid)
  #:use-module (gnu image)
  #:use-module (gnu system)
  #:use-module (gnu bootloader)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system linux-container)
  #:use-module (gnu system uuid)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services herd)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:export (guix-asahi
            read-operating-system
            service-node-type
            shepherd-service-node-type))

(define (process-command command args opts)
  "Process COMMAND, one of the 'guix system' sub-commands.  ARGS is its
argument list and OPTS is the option alist."
  (case command
    ;; The following commands do not need to use the store, and they do not need
    ;; an operating system configuration file.
    ((list-generations)
     (let ((list-installed-regex (assoc-ref opts 'list-installed))
           (pattern (match args
                      (() #f)
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (list-generations pattern #:list-installed-regex list-installed-regex)))
    ((describe)
     ;; Describe the running system, which is not necessarily the current
     ;; generation.  /run/current-system might point to
     ;; /var/guix/profiles/system-N-link, or it might point directly to
     ;; /gnu/store/…-system.  Try both.
     (let ((list-installed-regex (assoc-ref opts 'list-installed)))
       (match (generation-number "/run/current-system" %system-profile)
         (0
          (match (generation-number %system-profile)
            (0
             (leave (G_ "no system generation, nothing to describe~%")))
            (generation
             (display-system-generation
              generation #:list-installed-regex list-installed-regex))))
         (generation
          (display-system-generation
           generation #:list-installed-regex list-installed-regex)))))
    ((search)
     (apply (resolve-subcommand "search") args))
    ((edit)
     (apply (resolve-subcommand "edit") args))
    ;; The following commands need to use the store, but they do not need an
    ;; operating system configuration file.
    ((delete-generations)
     (let ((pattern (match args
                      (() #f)
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store* store
         (delete-matching-generations store %system-profile pattern)
         (reinstall-bootloader store (generation-number %system-profile)))))
    ((switch-generation)
     (let ((pattern (match args
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store* store
         (switch-to-system-generation store pattern))))
    ((roll-back)
     (let ((pattern (match args
                      (() "")
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store* store
         (roll-back-system store))))
    ;; The following commands need to use the store, and they also
    ;; need an operating system configuration file.
    (else (process-action command args opts))))

(define-command (guix-asahi . args)
  (synopsis "build and deploy full operating systems")
  (format #t (G_ "guix-asahi: command not yet implemented~%"))

  (define (parse-sub-command arg result)
    ;; Parse sub-command ARG and augment RESULT accordingly.
    (cond ((assoc-ref result 'action)
           (alist-cons 'argument arg result))
          ((member arg actions)
           (let ((action (string->symbol arg)))
             (alist-cons 'action action result)))
          (else
           (let ((hint (string-closest arg actions #:threshold 3)))
             (report-error (G_ "~a: unknown action~%") arg)
             (when hint
               (display-hint (G_ "Did you mean @code{~a}?~%") hint))
             (exit 1)))))

  ;; (define (match-pair car)
  ;;   ;; Return a procedure that matches a pair with CAR.
  ;;   (match-lambda
  ;;     ((head . tail)
  ;;      (and (eq? car head) tail))
  ;;     (_ #f)))

  ;; (define (option-arguments opts)
  ;;   ;; Extract the plain arguments from OPTS.
  ;;   (let* ((args   (reverse (filter-map (match-pair 'argument) opts)))
  ;;          (count  (length args))
  ;;          (action (assoc-ref opts 'action))
  ;;          (expr   (assoc-ref opts 'expression)))
  ;;     (define (fail)
  ;;       (leave (G_ "wrong number of arguments for action '~a'~%")
  ;;              action))

  ;;     (unless action
  ;;       (format (current-error-port)
  ;;               (G_ "guix system: missing command name~%"))
  ;;       (format (current-error-port)
  ;;               (G_ "Try 'guix system --help' for more information.~%"))
  ;;       (exit 1))

  ;;     (case action
  ;;       ((build container vm vm-image image disk-image docker-image
  ;;               reconfigure)
  ;;        (unless (or (= count 1)
  ;;                    (and expr (= count 0)))
  ;;          (fail)))
  ;;       ((init)
  ;;        (unless (= count 2)
  ;;          (fail))))
  ;;     args))

  (with-error-handling
    (let* ((opts (parse-command-line args %options
                                     (list %default-options)
                                     #:argument-handler
                                     parse-sub-command))
           (args (option-arguments opts))
           (command (assoc-ref opts 'action)))
      (with-status-verbosity (verbosity-level opts)
        (process-command command args opts)))))
