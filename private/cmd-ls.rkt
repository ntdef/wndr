#lang racket
(local-require "request.rkt")

(define (enumerate ls)
  (for/list ([idx (in-naturals 1)]
             [ith-ls ls])
    (cons idx ith-ls)))

;; (enumerate-map proc)

(define (cmd-ls-all-lists config)
  (let ([lists (map (compose wl-list->string hash->wl-list) (wl-lists config))])
    (for/list ([i (in-naturals 1)]
               [ith-list lists])
      (string-append "(" (~a (number->string i)
                             #:min-width 2
                             #:align 'right) ") " ith-list))))

(define ((cmd-ls-tasks-in-list ls) config)
  (match ls
    [(? number?) ()])
  )

(define my-client-id "")

(define my-access-token "")

(define my-config (config my-client-id my-access-token))

(for-each displayln (cmd-ls-all-lists my-config))


