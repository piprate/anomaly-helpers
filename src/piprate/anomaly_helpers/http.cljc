(ns piprate.anomaly-helpers.http
  (:require
   [cognitect.anomalies :as anom]
   [piprate.anomaly-helpers :as ah]))

(def code-to-cat {400 ::anom/incorrect   ;; Bad Request [RFC7231, Section 6.5.1]
                  401 ::anom/forbidden   ;; Unauthorized [RFC7235, Section 3.1]
                  402 ::anom/forbidden   ;; Payment Required [RFC7231, Section 6.5.2]
                  403 ::anom/forbidden   ;; Forbidden [RFC7231, Section 6.5.3]
                  404 ::anom/not-found   ;; Not Found [RFC7231, Section 6.5.4]
                  405 ::anom/unsupported ;; Method Not Allowed [RFC7231, Section 6.5.5]
                  406 ::anom/unsupported ;; Not Acceptable [RFC7231, Section 6.5.6]
                  407 ::anom/forbidden   ;; Proxy Authentication Required [RFC7235, Section 3.2]
                  408 ::anom/unavailable ;; Request Timeout [RFC7231, Section 6.5.7]
                  409 ::anom/conflict    ;; Conflict [RFC7231, Section 6.5.8]
                  410 ::anom/not-found   ;; Gone [RFC7231, Section 6.5.9]
                  411 ::anom/incorrect   ;; Length Required [RFC7231, Section 6.5.10]
                  412 ::anom/unavailable ;; ?? ;; Precondition Failed [RFC7232, Section 4.2][RFC8144, Section 3.2]
                  413 ::anom/incorrect   ;; Payload Too Large [RFC7231, Section 6.5.11]
                  414 ::anom/incorrect   ;; URI Too Long [RFC7231, Section 6.5.12]
                  415 ::anom/unsupported ;; Unsupported Media Type [RFC7231, Section 6.5.13][RFC7694, Section 3]
                  416 ::anom/incorrect   ;; Range Not Satisfiable [RFC7233, Section 4.4]
                  417 ::anom/unsupported ;; Expectation Failed [RFC7231, Section 6.5.14]
                  418 ::anom/unsupported ;; teapot
                  ;;419  ;;Unassigned
                  ;;420  ;;Unassigned
                  421 ::anom/incorrect   ;; Misdirected Request [RFC7540, Section 9.1.2]
                  422 ::anom/incorrect   ;; Unprocessable Entity [RFC4918]
                  423 ::anom/unavailable ;; Locked [RFC4918]
                  424 ::anom/unavailable ;; Failed Dependency [RFC4918]
                  ;;425 ;; Unassigned
                  426 ::anom/incorrect   ;; Upgrade Required [RFC7231, Section 6.5.15]
                  ;;427 ;; Unassigned
                  428 ::anom/incorrect   ;; Precondition Required [RFC6585]
                  429 ::anom/busy        ;; Too Many Requests [RFC6585]
                  ;;430 ;; Unassigned
                  431 ::anom/unsupported ;; Request Header Fields Too Large [RFC6585]
                  451 ::anom/unavailable ;; Unavailable For Legal Reasons [RFC7725]
                  500 ::anom/fault       ;; Internal Server Error [RFC7231, Section 6.6.1]
                  501 ::anom/unsupported ;; Not Implemented [RFC7231, Section 6.6.2]
                  502 ::anom/unavailable ;; Bad Gateway [RFC7231, Section 6.6.3]
                  503 ::anom/unavailable ;; Service Unavailable [RFC7231, Section 6.6.4]
                  504 ::anom/unavailable ;; Gateway Timeout [RFC7231, Section 6.6.5]
                  505 ::anom/unsupported ;; HTTP Version Not Supported [RFC7231, Section 6.6.6]
                  506 ::anom/fault       ;; Variant Also Negotiates [RFC2295]
                  507 ::anom/unavailable ;; Insufficient Storage [RFC4918]
                  508 ::anom/fault       ;; Loop Detected [RFC5842]
                  ;;509 ;; Unassigned
                  510 ::anom/fault       ;; Not Extended [RFC2774]
                  511 ::anom/forbidden   ;; Network Authentication Required [RFC6585]
                  })

(defn augment-anomalous [response]
  (if-let [status (:status response)]
    (cond
      (code-to-cat status) (ah/augment response (code-to-cat status))
      (<= 200 status 299)  response
      (<= 400 status 499)  (ah/augment response ::anom/incorrect)
      (<= 500 status 599)  (ah/augment response ::anom/fault)
      :else                response)
    response))
