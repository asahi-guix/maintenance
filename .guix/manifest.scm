(use-modules (guix)
             (guix profiles)
             (gnu packages base)
             (asahi-guix-maintenance-package)
             (asahi guix maintenance systems server))

;; (define %asahi-guix-version "0.1")

;; (define %asahi-guix-base
;;   (manifest-entry
;;     (name "asahi-guix-base")
;;     (version %asahi-guix-version)
;;     (item asahi-base-os)))

;; (packages->manifest (list asahi-guix-maintenance))

(packages->manifest (list asahi-guix-maintenance hello))
