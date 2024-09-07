(define-module (asahi guix maintenance services security)
  #:use-module (gnu services security)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(define-public %fail2ban-service
  (service fail2ban-service-type
           (fail2ban-configuration
            (extra-jails
             (list
              (fail2ban-jail-configuration
               (name "sshd")
               (enabled? #t)))))))
