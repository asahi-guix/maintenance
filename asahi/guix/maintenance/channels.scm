(define-module (asahi guix maintenance channels)
  #:use-module (guix channels))

(define-public %asahi-channel
  (channel
   (name 'asahi-channel)
   (branch "main")
   (url "https://github.com/asahi-guix/channel")
   (introduction
    (make-channel-introduction
     "3eeb493b037bea44f225c4314c5556aa25aff36c"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define-public %asahi-guix
  (channel
   (name 'guix)
   (url "https://github.com/asahi-guix/guix")
   (branch "main")
   (introduction
    (make-channel-introduction
     "59c86958338970cac132f45da37de3b00a26a8cc"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define-public %asahi-guix-next
  (channel
   (name 'guix)
   (url "https://github.com/asahi-guix/guix")
   (branch "next")
   (introduction
    (make-channel-introduction
     "8743e8ba7633769b07e08a26c43d255300e75096"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define-public %asahi-maintenance
  (channel
   (name 'asahi-maintenance)
   (branch "main")
   (url "https://github.com/asahi-guix/maintenance")
   (introduction
    (make-channel-introduction
     "c665797c5065cbed81b0c4a9e121baff1f2ffcc0"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define-public %gnu-guix
  (channel
   (name 'guix)
   (url "https://git.savannah.gnu.org/git/guix")
   (introduction
    (make-channel-introduction
     "9edb3f66fd807b096b48283debdcddccfea34bad"
     (openpgp-fingerprint
      "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

(define-public %gnu-guix-core-updates
  (channel
   (name 'guix)
   (url "https://git.savannah.gnu.org/git/guix")
   (branch "core-updates")
   (introduction
    (make-channel-introduction
     "9edb3f66fd807b096b48283debdcddccfea34bad"
     (openpgp-fingerprint
      "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

(define-public %r0man-channel
  (channel
   (name 'r0man-channel)
   (branch "main")
   (url "https://github.com/r0man/guix-channel")
   (introduction
    (make-channel-introduction
     "8eb7a76af9b51b80f5c01f18639e6360833fc377"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))
