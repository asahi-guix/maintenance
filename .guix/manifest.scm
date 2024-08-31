(use-modules (guix)
             (guix profiles)
             (asahi-guix-maintenance-package))

(display "ASAHI-GUIX-MANIFEST\n")

(packages->manifest (list asahi-guix-maintenance))
