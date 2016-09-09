#lang racket
(require net/url json)

(define (try-read-json in-port)
  (with-handlers ([exn:fail? (lambda (exn) (displayln (port->string in-port)))])
    (read-json in-port)))

(define (request-json-post data)
  (lambda (url [adtl-headers '()])
    (let ([payload (jsexpr->bytes data)]
          [header (cons "Content-Type: application/json"
                        adtl-headers)])
      (post-pure-port url
                      payload
                      header))))

(define (request-json-get)
  (lambda (url [header '()])
    (get-pure-port url (cons "Accept: application/json" header))))

(define-struct request (method url [data #:auto])
  #:auto-value #f
  #:transparent)

(define (request-method-pure-port request)
  (let ([data (request-data request)]
        [method (request-method request)])
    (case (request-method request)
      ['GET    get-pure-port]
      ['POST   (lambda (url [header '()]) (post-pure-port url data header))]
      ['PUT    put-pure-port]
      ['DELETE delete-pure-port])))

(define ((request-pure-port request) url [header '()])
  (let ([method (request-method-pure-port request)])
    (method url header)))

(define (fetch request [handle port->bytes])
  (call/input-url (string->url (request-url request))
                  (request-pure-port request)
                  handle))

(define (request-header request) '())

(define (request-header/json request)
  ;; By default, you're always going to want the `Accept` header
  (let ([header (cons "Accept: application/json" (request-header request))]
        [method (request-method request)])
    (case (request-method request)
      [('POST 'PUT) (cons "Content-Type: application/json" header)]
      [else header])))

(define (fetch-json request [handle read-json])
  (let ([header (request-header/json request)])
    (call/input-url (string->url (request-url request))
                    (request-pure-port request)
                    handle
                    header)))

(define my-get-req (make-request 'GET "https://httpbin.org/get"))

(curl/get)

(call/input-url (string->url "https://httpbin.org/post")
                (request-json-post "this is data")
                try-read-json)

(call/input-url (string->url "https://httpbin.org/get")
                (request-json-get)
                try-read-json)

(call/input-url (string->url "https://httpbin.org/get")
                (request-pure-port my-get-req)
                try-read-json)

(fetch my-get-req read-json)
(fetch-json my-get-req)
