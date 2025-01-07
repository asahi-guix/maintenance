(define-module (asahi guix maintenance services mcron)
  #:use-module (gnu services mcron)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (%mcron-service))

(define garbage-collector-job
  ;; Collect garbage 5 minutes after every hour.
  #~(job "5 * * * *" "guix gc -F 50G"))

(define %mcron-service
  (simple-service 'cron-jobs
                  mcron-service-type
                  (list garbage-collector-job)))
