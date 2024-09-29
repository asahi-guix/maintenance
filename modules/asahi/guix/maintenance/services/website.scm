(define-module (asahi guix maintenance services website)
  #:use-module (asahi guix packages installer)
  #:use-module (asahi guix packages website)
  #:use-module (gnu packages base)
  #:use-module (gnu services web)
  #:use-module (gnu services)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 match)
  #:use-module (guix gexp))

(define-record-type* <asahi-website-configuration>
  asahi-website-configuration
  make-asahi-website-configuration
  asahi-website-configuration?
  (package asahi-website-configuration-package
           (default asahi-guix-website))
  (deploy-directory asahi-website-deploy-directory
                    (default "/srv/http/www.asahi-guix.org"))
  (port asahi-website-configuration-port (default '("8880"))))

(define asahi-website-activation
  (match-lambda
    (($ <asahi-website-configuration> package deploy-directory port)
     #~(begin
         (use-modules (guix build utils))
         (when (directory-exists? #$deploy-directory)
           ;; Not 'delete-file-recursively' because the directory might be empty.
           (invoke (file-append #$coreutils "/bin/rm") "-r"
                   (string-append #$deploy-directory "/*")))
         (mkdir-p #$deploy-directory)
         (copy-recursively #$package #$deploy-directory)
         ;; (invoke (file-append #$coreutils "/bin/chmod") "a+w"
         ;;         (string-append #$deploy-directory "/sourcecodes/data"))
         ))))

(define asahi-website-nginx-config
  (match-lambda
    (($ <asahi-website-configuration> package deploy-directory port)
     (list
      (nginx-server-configuration
       (server-name '("Asahi Guix"))
       (listen port)
       (root deploy-directory)
       ;; (locations
       ;;  (list (nginx-php-location)))
       )))))

(define asahi-website-service-type
  (service-type
   (name 'asahi-website)
   (extensions
    (list
     (service-extension activation-service-type
                        asahi-website-activation)
     (service-extension nginx-service-type
                        asahi-website-nginx-config)
     ;; Make sure asahi-website doesn't get garbage collected.
     (service-extension profile-service-type
                        (compose list asahi-website-configuration-package))))
   (default-value (asahi-website-configuration))
   (description "Run the Asahi-Guix website.")))

(define-public %asahi-website-service
  (service asahi-website-service-type))
