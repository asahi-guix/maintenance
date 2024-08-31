(use-modules (guix)
             (guix profiles)
             (asahi-guix-maintenance-package))

(display "ASAHI-GUIX-MANIFEST")

(packages->manifest (list asahi-guix-maintenance))
