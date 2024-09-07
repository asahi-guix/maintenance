(define-module (asahi guix maintenance services base)
  #:use-module (gnu services base)
  #:use-module (gnu services))

(define-public %guix-publish-service
  (service guix-publish-service-type
           (guix-publish-configuration
            (compression '(("zstd" 3)))
            (port 8082))))
