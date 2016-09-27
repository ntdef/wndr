(declare (unit config))

(require-extension toml)

(define-record config client-id token)

(define (file->config fname)
  (with-input-from-file fname
    (lambda () (let ((data (read-toml (current-input-port))))
                 (apply make-config (map (lambda (k) (alist-ref k data))
                                         '(client-id token)))))))
