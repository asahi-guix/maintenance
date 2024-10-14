(define-module (asahi guix maintenance channels)
  #:use-module (guix channels))

(define-public %asahi-channel
  (channel
   (name 'asahi)
   (url "https://github.com/asahi-guix/channel")
   (branch "main")
   (introduction
    (make-channel-introduction
     "3eeb493b037bea44f225c4314c5556aa25aff36c"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define-public %asahi-gnu-guix-channel
  (channel
   (name 'guix)
   (url "https://github.com/asahi-guix/guix")
   (branch "main")
   (introduction
    (make-channel-introduction
     "9edb3f66fd807b096b48283debdcddccfea34bad"
     (openpgp-fingerprint
      "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

(define-public %asahi-maintenance-channel
  (channel
   (name 'asahi-maintenance)
   (url "https://github.com/asahi-guix/maintenance")
   (branch "main")
   (introduction
    (make-channel-introduction
     "c665797c5065cbed81b0c4a9e121baff1f2ffcc0"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define-public %gnu-guix-channel
  (channel
   (name 'guix)
   (url "https://git.savannah.gnu.org/git/guix.git")
   (introduction
    (make-channel-introduction
     "9edb3f66fd807b096b48283debdcddccfea34bad"
     (openpgp-fingerprint
      "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

(define-public %r0man-channel
  (channel
   (name 'r0man-channel)
   (url "https://github.com/r0man/guix-channel")
   (branch "main")
   (introduction
    (make-channel-introduction
     "8eb7a76af9b51b80f5c01f18639e6360833fc377"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define-public %r0man-home-channel
  (channel
   (name 'r0man-home)
   (url "https://github.com/r0man/guix-home")
   (branch "main")
   (introduction
    (make-channel-introduction
     "fdf26126b62dd922620ef3ce922b71180e57f455"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(list %asahi-channel
      %asahi-maintenance-channel
      %gnu-guix-channel)
