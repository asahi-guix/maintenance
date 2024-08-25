(use-modules (asahi guix maintenance systems server)
             (gnu packages)
             (gnu system)
             (guix gexp)
             (guix packages)
             (guix profiles))

(define %asahi-guix-version "0.0.1")

(define %asahi-guix-server
  (manifest-entry
    (name "asahi-guix-base")
    (version %asahi-guix-version)
    (item asahi-guix-server-system)))

(concatenate-manifests
 (list (manifest
        (list (specifications->manifest '("hello"))
              ;; %asahi-guix-edge
              ;; %asahi-guix-installer
              ))))
