(define-module (asahi guix maintenance services certbot)
  #:use-module (gnu services certbot)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (%certbot-deploy-hook))

(define %certbot-deploy-hook
  (program-file
   "certbot-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

(define-public (certbot-ssl-certificate domain)
  (format #f "/etc/certs/~a/fullchain.pem" domain))

(define-public (certbot-ssl-certificate-key domain)
  (format #f "/etc/certs/~a/privkey.pem" domain))

(define-public ci-asahi-guix-org-certificate
  (certificate-configuration
   (domains '("ci.asahi-guix.org"))
   (deploy-hook %certbot-deploy-hook)))

(define-public stats-asahi-guix-org-certificate
  (certificate-configuration
   (domains '("stats.asahi-guix.org"))
   (deploy-hook %certbot-deploy-hook)))

(define-public substitutes-asahi-guix-org-certificate
  (certificate-configuration
   (domains '("substitutes.asahi-guix.org"))
   (deploy-hook %certbot-deploy-hook)))

(define-public %certbot-service
  (service certbot-service-type
           (certbot-configuration
            (email "roman@asahi-guix.org")
            (certificates (list ci-asahi-guix-org-certificate
                                stats-asahi-guix-org-certificate
                                substitutes-asahi-guix-org-certificate)))))
