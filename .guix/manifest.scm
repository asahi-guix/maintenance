(use-modules (guix)
             (guix profiles)
             (gnu packages base)
             ;; (asahi-guix-maintenance-package)
             )

(display "ASAHI-GUIX-MANIFEST\n")

;; (packages->manifest (list asahi-guix-maintenance))

(packages->manifest (list hello))
