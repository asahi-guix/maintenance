(define-module (asahi guix maintenance services virtualization)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services)
  #:use-module (guix utils))

(define-public %qemu-service-x86-64
  (service qemu-binfmt-service-type
           (qemu-binfmt-configuration
            (platforms (lookup-qemu-platforms
                        (cond ((target-aarch64?)
                               "x86_64")
                              ((target-x86-64?)
                               "aarch64")))))))
