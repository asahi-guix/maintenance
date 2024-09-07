(define-module (asahi guix maintenance services certbot)
  #:use-module (gnu services)
  #:use-module (gnu services certbot)
  #:use-module (guix gexp))

(define-public (certbot-ssl-certificate domain)
  (format #f "/etc/certs/~a/fullchain.pem" domain))

(define-public (certbot-ssl-certificate-key domain)
  (format #f "/etc/certs/~a/privkey.pem" domain))

(define %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

(define-public ci-asahi-guix-org-certificate
  (certificate-configuration
   (domains '("ci.asahi-guix.org"))
   (deploy-hook %nginx-deploy-hook)))

(define-public substitutes-asahi-guix-org-certificate
  (certificate-configuration
   (domains '("substitutes.asahi-guix.org"))
   (deploy-hook %nginx-deploy-hook)))

(define-public www-asahi-guix-org-certificate
  (certificate-configuration
   (domains '("www.asahi-guix.org"))
   (deploy-hook %nginx-deploy-hook)))

(define-public %certbot-service
  (service certbot-service-type
           (certbot-configuration
            (email "roman@asahi-guix.org")
            (certificates (list ci-asahi-guix-org-certificate
                                substitutes-asahi-guix-org-certificate
                                www-asahi-guix-org-certificate)))))
