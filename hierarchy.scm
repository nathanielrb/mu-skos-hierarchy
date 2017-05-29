;; TODO
;; - what about those language filters??
;; - or get all properties??

(use awful srfi-69)

(load "sparql.scm")
(load "rest.scm")
(load "threads.scm")

(development-mode? #t)
(debug-file "./debug.log")
(*print-queries?* #t)

(*default-graph* (or (get-environment-variable "MU_DEFAULT_GRAPH")
                     '<http://data.europa.eu/eurostat/ECOICOP>))

(*sparql-endpoint* (or (get-environment-variable "SPARQL_ENDPOINT")
                       "http://172.31.63.185:8890/sparql?"))

(define-namespace taxonomy "http://data.europa.eu/eurostat/id/taxonomy/")

(define-namespace skos "http://www.w3.org/2004/02/skos/core#")


(define node-namespace (or "http://data.europa.eu/eurostat/id/taxonomy/ECOICOP/concept/"))

(define-namespace ns node-namespace)

(define scheme (make-parameter
                (or (get-environment-variable "CONCEPT_SCHEME")
                    (taxonomy "ECOICOP"))))

(define (descendance-query vars scheme child parent)
  (select-triples
   vars
   (format #f (conc "~A skos:inScheme ~A.~%"
                     "~A skos:broader ~A.~%")
           (reify child) (reify scheme) (reify child) (reify parent))))

(define (descendants-query node)
  (descendance-query "?x" (scheme) '?x node))

(define (ancestors-query node)
  (descendance-query "?x" (scheme) node '?x))

;; this shouldn't be hard-coded
;; get all properties? or...
(define (properties-query node)
  (select-triples
   "?name, ?description"
   (format #f (conc "~A skos:altLabel ?name.~%"
                     "~A skos:prefLabel ?description.~%"
                     "FILTER (lang(?name) = 'en')~%"
                     "FILTER (lang(?description) = 'en')~%")
           (reify node) (reify node))))

(define-syntax hit-cache
  (syntax-rules ()
    ((hit-property-cache sym prop body)
     (or (get sym prop)
         (put! sym prop body)))))

(define (get-descendants node)
  (let ((n node)) ;(string->symbol node)))
    (hit-cache n 'descendants
               (query-with-vars
                (x)
                (descendants-query node) ;(conc "<" node ">")) 
                x))))

(define (get-ancestors node)
  (let ((n node));  (string->symbol node)))
    (hit-cache n 'ancestors
               (query-with-vars 
                (x)
                (ancestors-query node);  (conc "<" node ">")) 
                x))))

(define (car-when l)
  (if (null? l) '() (car l)))

(define (node-properties node)
  (let ((n node)); (string->symbol node)))
    (hit-cache n 'properties
               (append (list (cons 'id (write-uri node)))
                       (car-when
                        (query-with-vars
                         (name description)
                         (properties-query node)
                         (list
                          (cons 'name name)
                          (cons 'description description))))))))

(define (tree next-fn node #!optional levels)
  (if (eq? levels 0)
      (node-properties node)
      (append (node-properties node)
              (list (cons 'children
                          (list->vector
                           (pmap-batch
                            100
                            (lambda (e)
                              (tree next-fn e (and levels (- levels 1))))
                            (next-fn node))))))))

(define (forward-tree node #!optional levels)
  (tree get-descendants node levels))

(define (reverse-tree node #!optional levels)
  (tree get-ancestors node levels))

(define-rest-page-fn (($path "/hierarchies/:id/descendants"))
  (lambda ()
    (let ((levels ($ 'levels)))
      `((data .
              ,(forward-tree
                (ns ($path 'id))
                (and levels (string->number levels))))))))

(define-rest-page-fn (($path "/hierarchies/:id/ancestors"))
  (lambda ()
    (let ((levels ($ 'levels)))
      (reverse-tree 
       (ns ($path 'id))
       (and levels (string->number levels))))))


