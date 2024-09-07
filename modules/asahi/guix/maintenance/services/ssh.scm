(define-module (asahi guix maintenance services ssh)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services ssh)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(define-public %openssh-service
  (service openssh-service-type
           (openssh-configuration
            (authorized-keys
             `(("root" ,(local-file "../files/ssh/authorized-keys/ed25519/roman.pub"))
               ("root" ,(local-file "../files/ssh/authorized-keys/rsa/root.pub"))
               ("root" ,(local-file "../files/ssh/authorized-keys/rsa/roman.pub"))
               ("roman" ,(local-file "../files/ssh/authorized-keys/ed25519/roman.pub"))
               ("roman" ,(local-file "../files/ssh/authorized-keys/rsa/roman.pub"))))
            (openssh openssh-sans-x)
            (permit-root-login 'prohibit-password)
            (port-number 22))))
