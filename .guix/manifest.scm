(use-modules (asahi guix maintenance packages guile-xyz)
             (asahi guix maintenance systems server)
             (gnu packages base)
             (gnu system)
             (guix gexp)
             (guix packages)
             (guix profiles)
             (guix profiles)
             (guix transformations))

(define %asahi-guix-server
  (manifest-entry
    (name "asahi-guix-server")
    (version "0.0.1")
    (item asahi-guix-server-system)))

(concatenate-manifests
 (list (packages->manifest (list asahi-guix-maintenance hello which))
       (manifest (list %asahi-guix-server))))
