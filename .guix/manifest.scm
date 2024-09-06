(use-modules (asahi guix maintenance packages guile-xyz)
             (asahi guix maintenance systems server)
             (gnu system)
             (guix gexp)
             (guix git-download)
             (guix packages)
             (guix profiles)
             (guix transformations))

(define vcs-file?
  (or (git-predicate "..") (const #t)))

(define source-checkout
  (local-file ".." "asahi-guix-maintenance-checkout"
              #:recursive? #t
              #:select? vcs-file?))

(define %asahi-guix-server
  (manifest-entry
    (name "asahi-guix-server")
    (version "0.0.1")
    (item asahi-guix-server-system)))

(define %asahi-guix-packages
  (packages->manifest
   (list (package
           (inherit asahi-guix-maintenance)
           (source source-checkout)))))

(concatenate-manifests
 (list (packages->manifest (list asahi-guix-maintenance))
       (manifest (list %asahi-guix-server))))
