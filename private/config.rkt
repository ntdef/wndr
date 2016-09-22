#lang racket/base
(require racket/file)

(struct config (client-id token))

(define (hash->config h)
  (apply config (map (λ (k) (hash-ref h k))
                     '(client-id token))))

(define (file->config fpath)
  ((compose hash->config make-hash file->list) fpath))

(provide config?
         config-client-id
         config-token
         file->config)
