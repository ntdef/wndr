(declare (uses config))

(load-relative "config.scm")

(require-extension http-client
                   uri-common
                   intarweb
                   medea)

(define (wl-path . path)
  (apply string-append "http://a.wunderlist.com/api/v1" path))

(define (path+params->uri path params)
  (uri-reference (wl-path (if params
                              (string-append path "?" (form-urlencode params))
                              path))))

(define (make-wl-request #!key config method path (params #f))
  (make-request uri: (path+params->uri path params)
                method: method
                headers: (headers `((x-client-id ,(config-client-id config))
                                    (x-access-token ,(config-token config))
                                    (accept application/json)
                                    (content-type application/json)))))

(define (wl-api-get config path #!optional (params #f))
  (with-input-from-request (make-wl-request config: config
                                            method: 'GET
                                            path: path
                                            params: params)
                           #f
                           read-json))

(define (wl-api-post config path data #!optional (params #f))
  (with-input-from-request (make-wl-request config: config
                                            method: 'POST
                                            path: path
                                            params: params)
                           data
                           read-json))

;; Example
;;
;; (wl-api-post (file->config "./api.conf")
;;              "/lists"
;;              (json->string '((title . "Hallo 2"))))
