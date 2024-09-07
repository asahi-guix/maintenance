(define-module (asahi guix maintenance services avahi)
  #:use-module (gnu services avahi)
  #:use-module (gnu services))

(define-public %avahi-service
  (service avahi-service-type))
