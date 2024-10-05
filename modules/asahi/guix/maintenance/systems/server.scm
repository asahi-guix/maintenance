(define-module (asahi guix maintenance systems server)
  #:use-module (asahi guix maintenance channels)
  #:use-module (asahi guix maintenance packages ci)
  #:use-module (asahi guix maintenance services admin)
  #:use-module (asahi guix maintenance services avahi)
  #:use-module (asahi guix maintenance services base)
  #:use-module (asahi guix maintenance services certbot)
  #:use-module (asahi guix maintenance services cuirass)
  #:use-module (asahi guix maintenance services databases)
  #:use-module (asahi guix maintenance services mcron)
  #:use-module (asahi guix maintenance services networking)
  #:use-module (asahi guix maintenance services security)
  #:use-module (asahi guix maintenance services ssh)
  #:use-module (asahi guix maintenance services virtualization)
  #:use-module (asahi guix maintenance services web)
  #:use-module (asahi guix maintenance services website)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system shadow)
  #:use-module (gnu system)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define %keyboard-layout
  (keyboard-layout "us" #:options '("caps:ctrl_modifier")))

(define %bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (keyboard-layout %keyboard-layout)
   (targets (list "/boot/efi"))))

(define %file-systems
  (cons* (file-system
           (mount-point "/")
           (device "/dev/sda1")
           (type "ext4")
           (needed-for-boot? #t))
         (file-system
           (mount-point "/boot/efi")
           (device "/dev/sda15")
           (type "vfat"))
         %base-file-systems))

(define %initrd-modules
  (append (cond ((target-aarch64?)
                 (list "sd_mod" "virtio_scsi"))
                (else '()))
          %base-initrd-modules))

(define %packages
  (cons* e2fsprogs
         emacs-minimal
         git
         htop
         net-tools
         screen
         %base-packages))

(define %users
  (cons (user-account
         (name "roman")
         (comment "Roman")
         (group "users")
         (home-directory "/home/roman")
         (supplementary-groups '("audio" "netdev" "video" "wheel")))
        %base-user-accounts))

(define %services
  (modify-services (cons* ;; %asahi-website-service
                          %avahi-service
                          %certbot-service
                          %cuirass-remote-worker-service
                          %cuirass-service
                          %fail2ban-service
                          %firewall-service
                          %guix-publish-service
                          %http-service
                          %mcron-service
                          %ntp-service
                          %openssh-service
                          %postgresql-service
                          %qemu-service-x86-64
                          %unattended-upgrade-service
                          (service dhcp-client-service-type)
                          %base-services)
    (guix-service-type
     config => (guix-configuration
                (inherit config)
                (authorized-keys
                 (cons* (local-file "../files/authorized-keys/apple-m1.pub")
                        (guix-configuration-authorized-keys config)))
                (channels (cons %asahi-guix-maintenance %default-channels))))))

(define %swap-devices
  (list (swap-space (target "/swapfile"))))

(define-public asahi-guix-server-system
  (operating-system
    (host-name "asahi-guix")
    (timezone "Etc/UTC")
    (locale "en_US.utf8")
    (kernel linux-libre)
    (bootloader %bootloader)
    (initrd-modules %initrd-modules)
    (file-systems %file-systems)
    (packages %packages)
    (services %services)
    (users %users)
    (swap-devices %swap-devices)))

asahi-guix-server-system
