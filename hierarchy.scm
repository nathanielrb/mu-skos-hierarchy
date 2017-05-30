(use awful srfi-69 irregex)

(load "s-sparql/sparql.scm")
(load "s-sparql/rest.scm")
(load "s-sparql/threads.scm")

(development-mode? #t)

(*print-queries?* #t)

(define-namespace skos "http://www.w3.org/2004/02/skos/core#")

(define lang
  (make-parameter
   (or (get-environment-variable "DEFAULT_LANGUAGE")
       "en")))

(define scheme
  (make-parameter
   (read-uri
    (get-environment-variable "CONCEPT_SCHEME"))))

(define *property-definitions*
  (or (get-environment-variable "INCLUDED_PROPERTIES")
      ""))

(define *properties*
  (map (lambda (str) 
         (map string->symbol (string-split str "="))) 
       (string-split *property-definitions* ",")))

(define (descendance-query vars scheme child parent)
  (select-triples
   vars
   (s-triples `((,child skos:inScheme ,scheme)
                (,child skos:broader ,parent)))))

(define (descendance-query vars scheme child parent)
  (select-triples
   (cons '?uuid vars)
   (s-triples `((,child skos:inScheme ,scheme)
                (,child skos:broader ,parent)
                (,child mu:uuid ?uuid)))))

(define (descendants-query node)
  (descendance-query '(?x) (scheme) '?x node))

(define (ancestors-query node)
  (descendance-query '(?x) (scheme) node '?x))

(define (get-node uuid)
  (query-unique-with-vars (node)
     (select-triples
      '?node
      (s-triples `((?node mu:uuid ,uuid))))
     node))

(define (get-top-concept)
  (query-unique-with-vars (node uuid)
     (select-triples
      '(?node ?uuid)
      (s-triples `((?node skos:topConceptOf ,(scheme))
                   (?node mu:uuid ?uuid))))
      (cons uuid node)))
         
(define (get-descendants node)
  (hit-property-cache node 'descendants
                      (query-with-vars
                       (uuid x)
                       (descendants-query node)
                       (cons uuid x))))

(define (get-ancestors node)
  (hit-property-cache node 'ancestors
             (query-with-vars 
              (uuid x)
              (ancestors-query node)
              (cons uuid x))))

(define (lang-or-none-filter var lang)
  (format #f "lang(~A) = '~A' || lang(~A) = ''" 
          var lang var))

(define property-query
  (match-lambda 
    ((name predicate)
     (let ((var (sparql-variable name)))
       (lambda (node)
         (cons var
               (s-optional
                (s-filter
                 (s-triple `(,node ,predicate ,var))
                 (lang-or-none-filter var (lang))))))))))

(define (properties-query node)
  (let* ((property-queries (map (lambda (pq) (pq node)) 
                                (map property-query *properties*)))
         (vars (map car property-queries))
         (queries (map cdr property-queries)))
    (let ((values (sparql/select-unique
                   (select-triples vars queries))))
      (filter (lambda (value-pair)
                (cdr value-pair))
              values))))

(define (node-properties node)
  (hit-property-cache 
   node 'properties
   (if-pair? *properties*
             (let ((properties (properties-query node)))
               properties))))

(define (tree next-fn node-pair #!optional levels #!key (relation 'children))
  (match-let (((uuid . node) node-pair))
             (if (eq? levels 0)
                 (node-properties node)
                 (let ((children (pmap-batch
                                  100
                                  (lambda (e)
                                    (tree next-fn e (and levels (- levels 1))))
                                  (next-fn node))))
                   (append `((id . ,uuid)
                             (type . "concept"))
                           (node-properties node)
                           (if-pair? children
                                     `((relationships 
                                        . ((,relation
                                            . ((data
                                                . ,(list->vector children)))))))))))))

(define (tree1 next-fn node #!optional levels #!key (relation 'children))
  (print node)
  (if (eq? levels 0)
      (node-properties (cdr node))
      (let ((children (pmap-batch
                       100
                       (lambda (e)
                         (tree next-fn e (and levels (- levels 1))))
                       (next-fn (cdr node)))))
        (if (null? children)
            (node-properties (cdr node))
            (append (node-properties (cdr node))
                    `((relationships 
                       . ((,relation
                           . ((data
                               . ,(list->vector children))))))))))))

(define (forward-tree uuid #!key levels)
  (let ((node (get-node uuid)))
    (vector (tree get-descendants node levels))))

(define (reverse-tree uuid #!optional levels)
  (let ((node (get-node uuid)))
    (vector (tree get-ancestors node levels #:relation 'parents))))

(define-rest-call (() "/hierarchies")
  (lambda ()
    `((data 
       . ,(tree get-descendants (get-top-concept) (str->num ($ 'levels)))))))

(define-rest-call ((id) "/hierarchies/:id/descendants")
  (lambda ()
    `((data 
       . ,(forward-tree id
                        #:levels (str->num ($ 'levels)))))))

(define-rest-call ((id) "/hierarchies/:id/ancestors")
  (lambda ()
    (reverse-tree 
     id (str->num ($ 'levels)))))

