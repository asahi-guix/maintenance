(define-module (asahi guix maintenance services cuirass)
  #:use-module (asahi guix maintenance channels)
  #:use-module (asahi guix maintenance packages ci)
  #:use-module (gnu services base)
  #:use-module (gnu services cuirass)
  #:use-module (gnu services)
  #:use-module (guix channels)
  #:use-module (guix gexp))

(define %cuirass-specifications
  #~(list (specification
           (name "asahi")
           (build '(custom (asahi guix ci)))
           (channels (list #$(channel->code %asahi-channel)
                           #$(channel->code %gnu-guix-channel)))
           (systems '("aarch64-linux"))
           (priority 2))
          (specification
           (name "maintenance")
           (build '(manifests ".guix/manifest.scm"))
           (channels (list #$(channel->code %asahi-channel)
                           #$(channel->code %asahi-maintenance-channel)
                           #$(channel->code %gnu-guix-channel)))
           (systems '("aarch64-linux"))
           (priority 1))
          (specification
           (name "r0man")
           (build '(manifests ".guix/manifest.scm"))
           (channels (list #$(channel->code %gnu-guix-channel)
                           #$(channel->code %r0man-home-channel)))
           (systems '("aarch64-linux"))
           (priority 3))))

(define-public %cuirass-service
  (service cuirass-service-type
           (cuirass-configuration
            (cuirass cuirass-disable-jit)
            (host "localhost")
            (port 8081)
            (specifications %cuirass-specifications)
            (ttl 259200)
            ;; (remote-server
            ;;  (cuirass-remote-server-configuration
            ;;   (private-key "/etc/guix/signing-key.sec")
            ;;   (public-key "/etc/guix/signing-key.pub")
            ;;   (publish? #f)
            ;;   ;; (trigger-url "http://localhost:8082")
            ;;   ))
            )))

(define-public %cuirass-remote-worker-service
  (service cuirass-remote-worker-service-type
           (cuirass-remote-worker-configuration
            (cuirass cuirass-disable-jit)
            (substitute-urls
             (cons* "https://substitutes.asahi-guix.org"
                    %default-substitute-urls))
            (systems '("aarch64-linux"))
            (workers 1))))
