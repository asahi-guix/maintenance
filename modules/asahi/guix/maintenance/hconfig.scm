(define-module (asahi guix maintenance hconfig)
  #:use-module (srfi srfi-26)
  #:export
  (%version
   %author
   %license
   %copyright
   %gettext-domain
   G_
   N_
   init-nls
   init-locale))

(define %version "0.1.0")

(define %author "Asahi Guix")

(define %license 'gpl3+)

(define %copyright '(2024))

(define %gettext-domain "asahi-guix-maintenance")

(define G_ identity)

(define N_ identity)

(define (init-nls) "Dummy as no NLS is used" #t)

(define (init-locale)
  "Dummy as no NLS is used"
  #t)
