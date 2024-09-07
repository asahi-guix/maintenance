(define-module (asahi guix maintenance services databases)
  #:use-module (gnu packages databases)
  #:use-module (gnu services databases)
  #:use-module (gnu services))

(define-public %postgresql-service
  (service postgresql-service-type
           (postgresql-configuration
            (postgresql postgresql-15))))
