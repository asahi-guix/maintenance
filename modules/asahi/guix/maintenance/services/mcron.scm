(define-module (asahi guix maintenance services mcron)
  #:use-module (gnu services mcron)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (%mcron-service))

(define garbage-collector-job
  ;; Collect garbage 5 minutes after midnight every day.
  #~(job "5 0 * * *" "guix gc -F 200G"))

(define %mcron-service
  (simple-service 'cron-jobs
                  mcron-service-type
                  (list garbage-collector-job)))
