(use srfi-13 http-client intarweb uri-common medea matchable)

(require-extension srfi-13)

(load "utilities.scm")

(define *default-graph* (make-parameter "http://tenforce.com/eurostat/"))

(define *sparql-endpoint* (make-parameter "http://localhost:8890/sparql"))

(define *print-queries?* (make-parameter #t))

(define *namespaces* (make-parameter '()))

(define (reify x)
    (cond ((string? x) (conc "\"" x "\""))
	  ((keyword? x) (keyword->string x))
	  ;;((list? x) (apply conc x))
          ((symbol? x) (symbol->string x))
	  ((namespace-pair? x) (expand-namespace x))))

(define (read-uri uri)
  (string->symbol (conc "<" uri ">")))

(define (write-uri uri)
  (let ((str (symbol->string uri)))
    (substring str 1 (- (string-length str) 1))))

(define (rdf->json x)
  (cond ((symbol? x) (write-uri x))
	(else x)))

(define format-triple
  (match-lambda 
    ((s p o)
     (format #f "~A ~A ~A .~%" s p o))))

;;  '(*top*
;;    (*namespaces* (foaf "http://foaf.org#")
;;                  (owl "http://owl.com/"))
;;    (*triples*
;;     (foaf:cake owl:likes foaf:icing)))
;; (define (full-triples triples)
;;  (let ((namespaces (assoc-val '*namespaces* (cdr triples)))
;;	(triples (assoc-val '*triples* (cdr triples))))

(define write-triple
  (match-lambda 
    ((s p o)
     (format-triple (map reify (list s p o))))))

(define (write-triples triples)
  (apply conc (map write-triple triples)))

(define (register-namespace name namespace)
  (*namespaces* (cons (list name namespace) (*namespaces*))))

(define (lookup-namespace name)
  (car-when (assoc-val name (*namespaces*))))

(define (expand-namespace ns-pair)
  (read-uri (format #f "~A~A" (lookup-namespace (car ns-pair)) (cadr ns-pair))))

(define (write-expand-namespace ns-pair)
  (format #f "~A~A" (lookup-namespace (car ns-pair)) (cadr ns-pair)))

(define (namespace-pair? x)
  (pair? x))

;; or consider a general function (expand-namespace mu 'pred)
(define-syntax define-namespace
  (syntax-rules ()
    ((define-namespace name namespace)
     (begin
       (register-namespace (quote name) namespace) ;  (->string
       (define (name elt)
         (read-uri (conc namespace elt)))))))

(define (insert-triples triples  #!optional (graph (*default-graph*)))
  (format #f "WITH <~A>~%INSERT {~%  ~A ~%}"
	  graph
	  triples))

(define (select-triples vars statements #!key (graph (*default-graph*)) order-by)
  (let ((order-statement (if order-by
			     (format #f "~%ORDER BY ~A" order-by)
			     "")))
    (format #f "WITH ~A~%SELECT ~A~%WHERE {~% ~A ~%} ~A"
	    graph vars statements order-statement)))

(define (expand-namespace-prefixes namespaces)
  (apply conc
	 (map (lambda (ns)
		(format #f "PREFIX ~A: <~A>~%"
			(car ns) (cadr ns)))
	      namespaces)))

(define (add-prefixes query)
  (format #f "~A~%~A"
	  (expand-namespace-prefixes (*namespaces*))
	  query))

(define (sparql/update query)
  (let ((endpoint (*sparql-endpoint*)))
    (when (*print-queries?*)
      (format #t "~%~%Query:~%~%~A" (add-prefixes query)))
    (let-values (((result uri response)
		  (with-input-from-request 
		   (make-request method: 'POST
				 uri: (uri-reference endpoint)
				 headers: (headers '((content-type application/sparql-update))))
		   (add-prefixes query)
		   read-string)))
      (close-connection! uri)
      response)))

(define (sparql/select query #!optional raw?)
  (let ((endpoint (*sparql-endpoint*)))
    (when (*print-queries?*)
	  (format #t "~%Query:~%~A~%" (add-prefixes query)))
    (let-values (((result uri response)
		  (with-input-from-request 
		   (make-request method: 'POST
				 uri: (uri-reference endpoint)
				 headers: (headers '((Content-Type application/x-www-form-urlencoded)
						     (Accept application/json))))
		   `((query . ,(add-prefixes query)))
                   read-json)))
      (close-connection! uri)
      (if raw? result (unpack-bindings result)))))

(define sparql-binding
  (match-lambda
    [(var (`type . "uri") . rest)
     (cons var (read-uri (assoc-val 'value rest)))]
    [(var (`type . "literal") . rest)
     (let ((lang (assoc-val 'xml:lang rest))
	   (value (assoc-val 'value rest)))
       (cons var (if lang (conc value "@" lang) value)))]
    [(var (`type . "typed-literal") . rest)
     (let ((datatype (assoc-val 'datatype rest))
	   (value (assoc-val 'value rest)))
       (match datatype
	 ("http://www.w3.org/2001/XMLSchema#integer"
	  (cons var (string->number value)))
	 (_ (cons var value))))]))

(define (unpack-bindings results)
  (map (lambda (binding)
	 (map sparql-binding binding))
	  (vector->list
	   (assoc-get 'bindings
		     (assoc-get 'results results)))))

;; (define-syntax query-with-bindings

(define-syntax query-with-vars
  (syntax-rules ()
    ((match-sparql (vars ...) query form)
     (map (match-lambda (((_ . vars) ...) form))
	  (sparql/select query)))))
