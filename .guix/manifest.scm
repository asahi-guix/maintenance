(use-modules (guix)
             (guix profiles)
             (gnu packages base)
             (asahi-guix-maintenance-package))

;; (packages->manifest (list asahi-guix-maintenance))

(packages->manifest (list asahi-guix-maintenance hello))
