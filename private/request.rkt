#lang racket
(require json
         net/uri-codec
         net/url)


(define (try-read-json in-port)
  (with-handlers ([exn:fail? (λ (exn) (displayln (port->string in-port)))])
    (read-json in-port)))

(define (request-json-post data)
  (λ (url [adtl-headers '()])
    (let ([payload (jsexpr->bytes data)]
          [header (cons "Content-Type: application/json"
                        adtl-headers)])
      (post-pure-port url
                      payload
                      header))))

(define (request-json-get)
  (λ (url [header '()])
    (get-pure-port url (cons "Accept: application/json" header))))

(struct request (method url data header)
  #:transparent)

(define make-request
  (case-lambda [(method url data header)
                (request method url data header)]
               [(method url data)
                (request method url data '())]
               [(method url)
                (request method url #f '())]
               [(url)
                (request 'GET url #f '())]))

(define (request-method-pure-port request)
  (let ([data (request-data request)]
        [method (request-method request)])
    (case (request-method request)
      ['GET    get-pure-port]
      ['POST   (λ (url [header '()]) (post-pure-port url data header))]
      ['PUT    put-pure-port]
      ['DELETE delete-pure-port])))

(define ((request-pure-port request) url [header '()])
  (let ([method (request-method-pure-port request)])
    (method url header)))


(define (request-header/json request)
  ;; By default, you're always going to want the `Accept` header
  (let ([header (cons "Accept: application/json" (request-header request))]
        [method (request-method request)])
    (case (request-method request)
      [(POST PUT) (cons "Content-Type: application/json" header)]
      [else header])))

(define (request-fetch request [handle port->bytes])
  (let ([header (request-header request)])
    (displayln header)
    (call/input-url (string->url (request-url request))
                    (request-pure-port request)
                    handle
                    header)))

(define (request-fetch/json request [handle read-json])
  (let ([header (request-header/json request)])
    (call/input-url (string->url (request-url request))
                    (request-pure-port request)
                    handle
                    header)))

(define my-get-req (make-request 'GET "https://httpbin.org/get"))

(define wl-request
  (make-request 'POST
                "http://localhost:9000"
                (string->bytes/utf-8 "hello!")))

(define (wl-auth-params this-id uri this-state)
  (list (cons 'client_id this-id)
        (cons 'redirect_uri uri)
        (cons 'state this-state)))

(define my-client-id "")

(define my-client-secret "")

(define my-secret-state "")

(define my-redirect-uri "")

(define my-params
  (wl-auth-params my-client-id my-redirect-uri my-secret-state))

(string-append "http://wunderlist.com/oauth/authorize?"
               (alist->form-urlencoded my-params))

(define my-auth-code "")

(define payload
  (hash 'client_id my-client-id
        'client_secret my-client-secret
        'code my-auth-code))

(define wl-first-req
  (make-request 'POST
                "https://www.wunderlist.com/oauth/access_token"
                (jsexpr->bytes payload)))

(define my-test-req
  (make-request 'POST
                "http://httpbin.org/post"
                (jsexpr->bytes payload)))

(define my-access-token
  (hash-ref (request-fetch/json wl-first-req) 'access_token))


(define (wl-api-header client-id token)
  (list (string-append "X-Access-Token: " token)
        (string-append "X-Client-ID: " client-id)))

(define my-header
  (wl-api-header my-client-id my-access-token))


(define (wl-path path)
  (string-append "http://a.wunderlist.com/api/v1" path))

(define (wl-lists config)
  (let* ([client-id (config-client-id config)]
         [token (config-token config)]
         [header (wl-api-header client-id token)]
         [path (wl-path "/lists")]
         ;; TODO Fix the arity options for make-request
         [wl-request (make-request 'GET path #"" header)])
    (request-fetch/json wl-request)))

(struct config (client-id token))

(define my-config (config my-client-id my-access-token))

(define my-lists (wl-lists my-config))

(define (wl-list-print wl-list port mode)
  (let ([title (wl-list-title wl-list)]
        [owner-id (number->string (wl-list-owner-id wl-list))]
        [id (number->string (wl-list-id wl-list))])
    (write-string (string-append "(" id ") " title) port)))

(struct wl-list (id
                 title
                 is-public
                 owner-id
                 list-type
                 owner-type
                 created-time
                 revision)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc wl-list-print)])

(define (hash->wl-list table)
  (let ([table-get (curry hash-ref table)]
        [wl-list-args (list 'id
                            'title
                            'public
                            'owner_id
                            'list_type
                            'owner_type
                            'created_at
                            'revision)])
    (apply wl-list (map table-get wl-list-args))))

(define (wl-list->string wl-list)
  (wl-list-title wl-list))

;; (map hash->wl-list my-lists)

(define (public? wl-list)
  (wl-list-is-public wl-list))

(define (title wl-list)
  (wl-list-title wl-list))

(hash->wl-list (car my-lists))

;; (Create Read Update Delete)

;; (define (wl-read conf resource))


(provide wl-lists
         wl-list->string
         hash->wl-list
         config)

