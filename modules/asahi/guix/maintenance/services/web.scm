(define-module (asahi guix maintenance services web)
  #:use-module (asahi guix maintenance packages web)
  #:use-module (asahi guix maintenance services certbot)
  #:use-module (gnu services web)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(define-public %http-service-bootstrap
  (service
   nginx-service-type
   (nginx-configuration
    (server-blocks
     (list
      (nginx-server-configuration
       (locations
        (list
         (nginx-location-configuration
          (uri "/")
          (body '("return 404;")))))))))))

(define-public %http-service
  (service
   nginx-service-type
   (nginx-configuration
    (modules
     (list
      (file-append nginx-auth-jwt-module "/etc/nginx/modules/ngx_http_auth_jwt_module.so")))
    (server-blocks
     (list
      (nginx-server-configuration
       (server-name '("ci.asahi-guix.org"))
       (listen '("443 ssl" "[::]:443 ssl"))
       (ssl-certificate (certbot-ssl-certificate "ci.asahi-guix.org"))
       (ssl-certificate-key (certbot-ssl-certificate-key "ci.asahi-guix.org"))
       (locations
        (list
         (nginx-location-configuration
          (uri "~ ^/admin/forgejo/event")
          (body (list "proxy_pass http://cuirass;"
                      "auth_jwt \"Cuirass Webhook\";"
                      "auth_jwt_key_file \"/root/.config/asahi-guix/asahi-guix-maintenance.webhook.jwks\";"
                      ;; TODO: Enable auth_jwt_validate_exp
                      "auth_jwt_validate_exp off;")))
         (nginx-location-configuration
          (uri "~ ^/admin")
          (body (list "if ($ssl_client_verify != SUCCESS) { return 403; } proxy_pass http://cuirass;")))
         (nginx-location-configuration
          (uri "/")
          (body '("proxy_pass http://cuirass;"))))))
      (nginx-server-configuration
       (server-name '("stats.asahi-guix.org"))
       (listen '("443 ssl" "[::]:443 ssl"))
       (ssl-certificate (certbot-ssl-certificate "stats.asahi-guix.org"))
       (ssl-certificate-key (certbot-ssl-certificate-key "stats.asahi-guix.org"))
       (locations
        (list
         (nginx-location-configuration
          (uri "/")
          (body '("proxy_pass http://asahi-stats;"))))))
      (nginx-server-configuration
       (server-name '("substitutes.asahi-guix.org"))
       (listen '("443 ssl" "[::]:443 ssl"))
       (ssl-certificate (certbot-ssl-certificate "substitutes.asahi-guix.org"))
       (ssl-certificate-key (certbot-ssl-certificate-key "substitutes.asahi-guix.org"))
       (locations
        (list
         (nginx-location-configuration
          (uri "/")
          (body '("proxy_pass http://guix-publish;"))))))))
    (upstream-blocks
     (list (nginx-upstream-configuration
            (name "asahi-stats")
            (servers (list "127.0.0.1:8000")))
           (nginx-upstream-configuration
            (name "cuirass")
            (servers (list "127.0.0.1:8081")))
           (nginx-upstream-configuration
            (name "guix-publish")
            (servers (list "127.0.0.1:8082"))))))))
