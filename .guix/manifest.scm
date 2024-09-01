(use-modules (guix)
             (guix profiles)
             (gnu packages base)
             (asahi guix maintenance packages guile-xyz))

;; (packages->manifest (list asahi-guix-maintenance))

(packages->manifest (list asahi-guix-maintenance hello))
