(define-module (asahi guix maintenance manifest)
  #:use-module (asahi guix maintenance systems server)
  #:use-module (gnu packages)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles))

(define %asahi-guix-version "0.0.1")

(define %asahi-guix-server
  (manifest-entry
    (name "asahi-guix-server")
    (version %asahi-guix-version)
    (item asahi-guix-server-system)))

;; (concatenate-manifests
;;  (list (manifest (list %asahi-guix-server))))

(manifest (list %asahi-guix-server))
