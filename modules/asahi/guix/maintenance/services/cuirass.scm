(define-module (asahi guix maintenance services cuirass)
  #:use-module (asahi guix maintenance channels)
  #:use-module (asahi guix maintenance packages ci)
  #:use-module (gnu services cuirass)
  #:use-module (gnu services)
  #:use-module (guix channels)
  #:use-module (guix gexp))

(define %cuirass-specifications
  #~(list (specification
           (name "asahi-guix-channel")
           ;; (build '(channels asahi-guix-channel))
           (build '(custom (asahi guix ci)))
           (channels (list #$(channel->code %asahi-guix-channel)
                           #$(channel->code %gnu-guix)))
           (build-outputs
            (list (build-output
                   (job "asahi-installer-image*")
                   (type "archive")
                   (output "out")
                   (path ""))))
           (systems '("aarch64-linux"))
           (priority 2))
          (specification
           (name "asahi-guix-maintenance")
           (build '(manifests ".guix/manifest.scm"))
           (channels (list #$(channel->code %asahi-guix-maintenance)
                           #$(channel->code %gnu-guix)))
           (systems '("aarch64-linux"))
           (priority 1))
          (specification
           (name "r0man-channel")
           (build '(channels r0man-channel))
           (channels (list #$(channel->code %gnu-guix)
                           #$(channel->code %r0man-channel)))
           (systems '("aarch64-linux"))
           (priority 4))
          (specification
           (name "r0man-home")
           (build '(manifests ".guix/manifest.scm"))
           (channels (list #$(channel->code %gnu-guix)
                           #$(channel->code %r0man-home)))
           (systems '("aarch64-linux"))
           (priority 5))))

(define-public %cuirass-service
  (service cuirass-service-type
           (cuirass-configuration
            (cuirass cuirass-disable-jit)
            (host "localhost")
            (port 8081)
            (specifications %cuirass-specifications)
            (remote-server
             (cuirass-remote-server-configuration
              (backend-port 5555)
              (log-port 5556)
              (publish-port 5557))))))

(define-public %cuirass-remote-worker-service
  (service cuirass-remote-worker-service-type
           (cuirass-remote-worker-configuration
            (cuirass cuirass-disable-jit)
            (systems '("aarch64-linux"))
            (workers 4))))