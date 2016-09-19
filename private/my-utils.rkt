#lang racket

(define verbose-mode (make-parameter #f))

(define (dispatch-cmd cmd argv)
  (if (eq? cmd "cmd")
      (command-line #:argv argv
                    #:once-each
                    [("-v" "--verbose") "Run in verbose mode" (verbose-mode #t)])
      '())
  (displayln (verbose-mode)))

(define (multi-command [argv (current-command-line-arguments)])
  (match-let ([(cons cmd flags) (command-line #:argv argv #:args opts opts)])
    (displayln cmd)
    (dispatch-cmd cmd flags)))


(display (multi-command (vector "cmd" "-v" "-f")))
