#lang racket
(require json
         net/uri-codec
         net/url)

(define (try-read-json in-port)
  (with-handlers ([exn:fail? (λ (exn) (displayln (port->string in-port)))])
    (read-json in-port)))

(struct request (method url data header) #:transparent)

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

(define (make-wl-request-helper config method path data)
  (let* ([client-id  (config-client-id config)]
         [token      (config-token config)]
         [header     (wl-api-header client-id token)]
         [path       (wl-path path)]
         [wl-request (make-request method path #"" header)])
    wl-request))

(define make-wl-request
  (let ([helper-fn (λ (config method path #:data [data #""])
                     (let* ([client-id  (config-client-id config)]
                         [token      (config-token config)]
                         [header     (wl-api-header client-id token)]
                         [path       (wl-path path)]
                         [wl-request (make-request method path data header)])
                    wl-request))])
    (case-lambda [(config path) (helper-fn config 'GET path)]
                 [(config method path) (helper-fn config method path)])))

(define (wl-auth-params this-id uri this-state)
  (list (cons 'client_id this-id)
        (cons 'redirect_uri uri)
        (cons 'state this-state)))

(define (wl-api-header client-id token)
  (list (string-append "X-Access-Token: " token)
        (string-append "X-Client-ID: " client-id)))

(define (wl-path . path)
  (apply string-append "http://a.wunderlist.com/api/v1" path))

(define (wl-api-get config path #:params [params #f])
  (let ([path* (if params
                   (string-append path "?" (alist->form-urlencoded params))
                   path)])
    (request-fetch/json (make-wl-request config 'GET path*))))

(define (wl-api-get-lists config [list-id #f])
  (let ([path (cond
                [(string? list-id) (string-append "/lists/" list-id)]
                [(number? list-id) (string-append "/lists/" (number->string list-id))]
                [else "/lists"])])
    (wl-api-get config path)))

(define (wl-api-get-tasks config list-id)
  (let ([path "/tasks"]
        [list-id (if (number? list-id) (number->string list-id) list-id)]
        [params (list (cons 'list_id list-id))])
    (wl-api-get config path #:params params)))

(define (wl-api-get-folders config)
  (wl-api-get config "/folders"))

;; -- Config Struct
(struct config (client-id token))

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

(define (public? wl-list)
  (wl-list-is-public wl-list))

(define (title wl-list)
  (wl-list-title wl-list))

(define my-client-id "")

(define my-access-token "")

(define my-config (config my-client-id my-access-token))

(define my-lists (wl-api-get-lists my-config))

;; (Create Read Update Delete)
;; (define (wl-read conf resource))

(provide wl-list->string
         hash->wl-list
         config)
