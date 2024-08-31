(use-modules (guix)
             (guix profiles)
             (asahi-guix-maintenance-package))

(packages->manifest (list asahi-guix-maintenance))
