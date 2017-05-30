;; TODO
;; - what about those language filters??
;; - or get all properties??

(use awful srfi-69 irregex)

(load "sparql.scm")
(load "rest.scm")
(load "threads.scm")

(development-mode? #t)
(debug-file "./debug.log") ;; **conf
(*print-queries?* #t)

(define-namespace skos "http://www.w3.org/2004/02/skos/core#")
(define-namespace mu "http://mu.semte.ch/vocabularies/core/")

(*default-graph* (or (get-environment-variable "MU_DEFAULT_GRAPH")
                     '<http://data.europa.eu/eurostat/ECOICOP>))

(*sparql-endpoint* (or (get-environment-variable "SPARQL_ENDPOINT")
                       "http://172.31.63.185:8890/sparql?"))

(define scheme (make-parameter
                (or (get-environment-variable "CONCEPT_SCHEME")
                    '<http://data.europa.eu/eurostat/id/taxonomy/ECOICOP>)))

(define *property-definitions*
  (or (get-environment-variable "INCLUDED_PROPERTIES")
      "name=skos:altLabel,description=skos:prefLabel,notation=skos:notation"))

(define *properties*
  (map (lambda (str) 
         (map string->symbol (string-split str "="))) 
       (string-split *property-definitions* ",")))

(define *namespace-definitions*
  (or (get-environment-variable "NAMESPACES")
      "skos: http://www.w3.org/2004/02/skos/core#"))

(define *defined-namespaces*
  (map (lambda (ns) 
         (print ns)
         (print (irregex-split ": " ns))
         (match-let (((prefix uri)
                      (irregex-split ": " ns)))
           (register-namespace (string->symbol prefix)                                
                               uri)))
       (string-split *namespace-definitions* ",")))
       
(define-syntax hit-cache
  (syntax-rules ()
    ((hit-property-cache sym prop body)
     (or (get sym prop)
         (put! sym prop body)))))
    
(define (descendance-query vars scheme child parent)
  (select-triples
   vars
   (s-triples `((,child skos:inScheme ,scheme)
                (,child skos:broader ,parent)))))

(define (descendants-query node)
  (descendance-query "?x" (scheme) '?x node))

(define (ancestors-query node)
  (descendance-query "?x" (scheme) node '?x))

(define (get-node uuid)
  (query-unique-with-vars (node)
     (select-triples
      "?node"
      (s-triples `((?node mu:uuid ,uuid))))
     node))
         
(define (get-descendants node)
  (hit-cache node 'descendants
             (query-with-vars
              (x)
              (descendants-query node) ;(conc "<" node ">")) 
              x)))

(define (get-ancestors node)
  (hit-cache node 'ancestors
             (query-with-vars 
              (x)
              (ancestors-query node)
              x)))

(define property-query
  (match-lambda 
    ((name predicate)
     (let ((var (sparql-variable name)))
       (lambda (node)
         (cons var
               (s-optional
                (s-filter
                 (s-triple `(,node ,predicate ,var))
                 (format #f "lang(~A) = 'en' || lang(~A) = ''" var var)))))))))

(define (properties-query node)
  (let* ((property-queries (map (lambda (pq) (pq node)) 
                                (map property-query *properties*)))
         (vars (cons "?uuid" (map ->string (map car property-queries))))
         (queries (cons (s-triple `(,node mu:uuid ?uuid))
                        (map cdr property-queries))))
    (let ((values (car-when
                   (sparql/select
                    (select-triples
                     (string-join vars ", ")
                     (string-join queries "\n"))))))
      (filter (lambda (value-pair)
                (cdr value-pair))
              values))))

(define (node-properties node)
  (hit-cache 
   node 'properties
   (let ((properties (properties-query node)))
     `((id . ,(alist-ref 'uuid properties))
       (type . "concept")
       (attributes . ,(alist-delete 'uuid properties))))))
             
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
                ($path 'id)
                (and levels (string->number levels))))))))

(define-rest-page-fn (($path "/hierarchies/:id/ancestors"))
  (lambda ()
    (let ((levels ($ 'levels)))
      (reverse-tree 
        ($path 'id)
       (and levels (string->number levels))))))


