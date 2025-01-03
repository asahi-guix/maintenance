(define-module (asahi guix maintenance machines server)
  #:use-module (asahi guix maintenance systems server)
  #:use-module (gnu machine ssh)
  #:use-module (gnu machine)
  #:use-module (gnu)
  #:export (asahi-guix-server-machine))

(define host-key
  (string-append "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFZPlAeMk4hiHKHDCoDd1bT/ddMaZZR0iMWHE/mCGDFX root@(none)"))

(define asahi-guix-server-machine
  (machine
   (operating-system asahi-guix-server-system)
   (environment managed-host-environment-type)
   (configuration (machine-ssh-configuration
                   (allow-downgrades? #t)
                   (build-locally? #f)
                   (host-key host-key)
                   (host-name "www.asahi-guix.org")
                   (port 22)
                   (system "aarch64-linux")
                   (user "root")))))

(list asahi-guix-server-machine)
