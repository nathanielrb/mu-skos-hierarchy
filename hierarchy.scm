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
(define-namespace mu "http://mu.semte.ch/vocabularies/core/")
(define-namespace concept "http://data.europa.eu/eurostat/id/taxonomy/ECOICOP/concept/")

(define node-namespace (or "http://data.europa.eu/eurostat/id/taxonomy/ECOICOP/concept/"))

(define-namespace ns node-namespace)

(define scheme (make-parameter
                (or (get-environment-variable "CONCEPT_SCHEME")
                    (taxonomy "ECOICOP"))))

(define (descendance-query vars scheme child parent)
  (select-triples
   vars
   (s-triples `((,child skos:inScheme ,scheme)
               (,child skos:broader ,parent)))))

(define (descendants-query node)
  (descendance-query "?x" (scheme) '?x node))

(define (ancestors-query node)
  (descendance-query "?x" (scheme) node '?x))

;; this shouldn't be hard-coded
;; get all properties? or...
(define (properties-query node)
  (select-triples
   "?name, ?description, ?uuid"
   (conc (s-optional (conc (s-triple `(,node skos:altLabel ?name))
                           "FILTER (lang(?name) = 'en')\n"))
         (s-optional (conc (s-triple `(,node skos:prefLabel ?description))
                           "FILTER (lang(?description) = 'en')\n"))
         (s-triple `(,node mu:uuid ?uuid)))))
         


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
               (append (list (cons 'id (write-uri node))
                             (cons 'type "TYPE")
                             (cons 'attributes
                                   (car-when
                                    (query-with-vars
                                     (name description uuid)
                                     (properties-query node)
                                     (list
                                      (cons 'id uuid)
                                      (cons 'name name)
                                      (cons 'description description))))))))))
(define (node-properties node)
  (let ((n node)); (string->symbol node)))
    (hit-cache n 'properties
               (car 
                (query-with-vars
                 (name description uuid)
                 (properties-query node)
                 `((@id . ,(write-uri node))
                   (id . ,uuid)
                   (type . "concept")
                   (attributes .
                     (,@(if name `((name . ,name)) '())
                      ,@(if description `((description . ,description)) '())))))))))


(define (tree next-fn node #!optional levels)
  (if (eq? levels 0)
      (node-properties node)
      (let ((children (pmap-batch
                       100
                       (lambda (e)
                         (tree next-fn e (and levels (- levels 1))))
                       (next-fn node))))
        (if (null? children)
            (node-properties node)
            (append (node-properties node)
                    `((relationships .
                                     ((children .
                                                ((data .
                            ,(list->vector children))))))))))))

(define (forward-tree node #!optional levels)
  (tree get-descendants node levels))

(define (get-node uuid)
  (car
   (query-with-vars (node)
     (select-triples
      "?node"
      (s-triples `((?node mu:uuid ,uuid))))
      node)))

(define (forward-tree uuid #!optional levels)
  (let ((node (get-node uuid)))
    (tree get-descendants node levels)))

(define (reverse-tree uuid #!optional levels)
  (let ((node (get-node uuid)))
    (tree get-ancestors node levels)))

(define-rest-page-fn (($path "/hierarchies/:id/descendants"))
  (lambda ()
    (let ((levels ($ 'levels)))
      `((data .
              ,(forward-tree
                ($path 'id) ;;(ns ($path 'id))
                (and levels (string->number levels))))))))

(define-rest-page-fn (($path "/hierarchies/:id/ancestors"))
  (lambda ()
    (let ((levels ($ 'levels)))
      (reverse-tree 
        ($path 'id)
       (and levels (string->number levels))))))


