(define-module (asahi guix maintenance services admin)
  #:use-module (asahi guix maintenance channels)
  #:use-module (gnu services admin)
  #:use-module (gnu services)
  #:use-module (guix channels)
  #:use-module (guix gexp))

(define-public %unattended-upgrade-service
  (service unattended-upgrade-service-type
           (unattended-upgrade-configuration
            (channels #~(list #$(channel->code %gnu-guix)
                              #$(channel->code %asahi-guix-maintenance)))
            (schedule "0 4 * * *")
            ;; (schedule "*/3 * * * *")
            (services-to-restart
             '(avahi-daemon
               console-font-tty1
               console-font-tty2
               console-font-tty3
               console-font-tty4
               console-font-tty5
               console-font-tty6
               cuirass
               cuirass-remote-server
               cuirass-remote-worker
               cuirass-web
               dbus-system
               fail2ban
               guix-daemon
               guix-publish
               mcron
               nginx
               nscd
               pam
               postgres
               qemu-binfmt
               ssh-daemon
               syslogd
               term-console
               term-tty1
               term-tty2
               term-tty3
               term-tty4
               term-tty5
               term-tty6
               virtual-terminal)))))
