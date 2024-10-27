(define-module (asahi guix maintenance services website)
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module (asahi guix build modules)
  #:use-module (asahi guix maintenance packages website)
  #:use-module (asahi guix maintenance services certbot)
  #:use-module (asahi guix maintenance services web)
  #:use-module (asahi guix packages installer)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu services certbot)
  #:use-module (gnu services mcron)
  #:use-module (gnu services web)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (%asahi-website-service
            asahi-website-configuration
            asahi-website-configuration-contact
            asahi-website-configuration-package
            asahi-website-configuration-port
            asahi-website-configuration-server-name
            asahi-website-configuration?
            asahi-website-deploy-directory
            asahi-website-service-type
            make-asahi-website-configuration))

(define-record-type* <asahi-website-configuration>
  asahi-website-configuration
  make-asahi-website-configuration
  asahi-website-configuration?
  (contact asahi-website-configuration-contact
           (default "roman@asahi-guix.org"))
  (deploy-directory asahi-website-deploy-directory
                    (default "/srv/http"))
  (package asahi-website-configuration-package
           (default asahi-guix-website))
  (port asahi-website-configuration-port (default '("8880")))
  (server-name asahi-website-configuration-server-name
               (default "www.asahi-guix.org")))

(define asahi-website-activation
  (match-lambda
    (($ <asahi-website-configuration>
        contact deploy-directory package port server-name)
     #~(begin
         (use-modules (guix build utils))
         (let ((target (string-append #$deploy-directory "/" #$server-name))
               (source (string-append #$package "/share/asahi-guix-website")))
           (when (directory-exists? target)
             (invoke #$(file-append coreutils "/bin/rm") "-r" target))
           (mkdir-p target)
           (copy-recursively source target))))))

(define asahi-website-certbot-config
  (match-lambda
    (($ <asahi-website-configuration>
        contact deploy-directory package port server-name)
     (list (certificate-configuration
            (domains (list server-name))
            (deploy-hook %certbot-deploy-hook))))))

(define (asahi-website-mcron-config config)
  (with-extensions (list guile-gcrypt guile-json-4 guile-zlib)
    (with-imported-modules `(((guix config) => ,(make-config.scm))
                             ,@(source-module-closure
                                '((asahi guix maintenance build website))
                                #:select? import-asahi-module?))
      (list #~(job "*/1 * * * *"
                   #$(program-file
                      "build-website.scm"
                      #~(begin
                          (use-modules (asahi guix maintenance build website))
                          (format #t "Building website ...~%")
                          (build-website
                           (website-builder
                            (output-dir "/srv/http/www.asahi-guix.org/builds")
                            (script-path #$(file-append asahi-installer-script "/bin/asahi-guix-install"))))
                          (format #t "Successfully built website.~%"))))))))

(define asahi-website-nginx-config
  (match-lambda
    (($ <asahi-website-configuration>
        contact deploy-directory package port server-name)
     (let ((cert (certbot-ssl-certificate server-name))
           (key (certbot-ssl-certificate-key server-name))
           (root (string-append deploy-directory "/" server-name)))
       (list (nginx-server-configuration
              (server-name (list server-name))
              (listen '("443 ssl" "[::]:443 ssl"))
              (ssl-certificate cert)
              (ssl-certificate-key key)
              (root root)))))))

(define asahi-website-profile-config
  (match-lambda
    (($ <asahi-website-configuration>
        contact deploy-directory package port server-name)
     (list coreutils package))))

(define asahi-website-service-type
  (service-type
   (name 'asahi-website)
   (extensions
    (list
     (service-extension activation-service-type
                        asahi-website-activation)
     (service-extension certbot-service-type
                        asahi-website-certbot-config)
     (service-extension mcron-service-type
                        asahi-website-mcron-config)
     (service-extension nginx-service-type
                        asahi-website-nginx-config)
     ;; Make sure the website doesn't get garbage collected.
     (service-extension profile-service-type
                        asahi-website-profile-config)))
   (default-value (asahi-website-configuration))
   (description "Run the Asahi-Guix website.")))

(define %asahi-website-service
  (service asahi-website-service-type))
