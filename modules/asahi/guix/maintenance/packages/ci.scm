(define-module (asahi guix maintenance packages ci)
  #:use-module ((gnu packages ci) #:prefix ci:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages))

(define %disable-jit-patch
  (local-file "../patches/cuirass-disable-jit.patch"))

(define-public cuirass-disable-jit
  (package
    (inherit (package-with-patches ci:cuirass (list %disable-jit-patch)))
    (name "cuirass-disable-jit")))