(use spiffy spiffy-request-vars srfi-69 matchable irregex medea
     s-sparql mu-chicken-support
     http-client intarweb uri-common
     cjson)

(define *lang*
  (config-param "DEFAULT_LANGUAGE" "en"))

(define *json-api-type*
  (config-param "JSON_API_TYPE" "concept"))

(define *format*
  (config-param "DATA_FORMAT" "json-api"))

(define *batch-levels*
  (config-param "BATCH_LEVELS" 6))

(define *scheme*
  (config-param "CONCEPT_SCHEME" #f read-uri))

(define *schemes*
  (config-param "CONCEPT_SCHEMES" #f
                (lambda (scheme-list)
                  (and (pair? scheme-list)
                       (map read-uri
                            (string-split scheme-list ","))))))

(define *property-definitions*
  (config-param "INCLUDED_PROPERTIES" ""))

(define *concept-scheme-type*
  (config-param "CONCEPT_SCHEME_TYPE" 'skos:ConceptScheme read-uri))

(define *concept-type* (config-param "CONCEPT_TYPE" 'skos:Concept read-uri))

(define *top-concept-predicate*
  (config-param "TOP_CONCEPT_PREDICATE" 'skos:topConceptOf read-uri))

(define *broader-predicate*
  (config-param "BROADER_PREDICATE" 'skos:broader read-uri))

(define *in-scheme-predicate*
  (config-param "IN_SCHEME_PREDICATE" 'skos:inScheme read-uri))

(define (split-properties property-definitions)
  (map (lambda (str) 
         (map string->symbol (string-split str " "))) 
       (string-split property-definitions ",")))

(define *properties*
  (make-parameter
   (split-properties (*property-definitions*))))

(define *cache* (make-hash-table))

(define (lang-or-none-filter var lang)
  `(FILTER (|@()| (LANG ,var) = ,lang |\|\|| (LANG ,var) = "")))

;; clean this up, please
(define (query-statements schemes uuid properties lang)
  (let* ((P (lambda (n)
             (join
              (map (lambda (property)
                     (let ((var (symbol-append '? (second property) n)))
                       `((,(symbol-append '?child n) ,(car property) ,var)
                         ,(lang-or-none-filter var lang))))
                   properties))))
        (Vs (lambda (n)
              (map (lambda (property)
                     (symbol-append '? (second property) n))
                   properties)))
        (G (lambda (n bindings)
             (map (lambda (property)
                    (let ((p (second property)))
                      `(,p . ,(alist-ref (symbol-append p n) bindings))))
                  properties))))
    (map (lambda (bindings)
           `(,(alist-ref 'child0 bindings) 
             . ((id . ,(alist-ref 'uuid0 bindings))
                (attributes
                 . (,@(G '|0| bindings)))
                (,(alist-ref 'child1 bindings)
                 . ((id . ,(alist-ref 'uuid1 bindings))
                    (attributes
                     . (,@(G '|1| bindings))))))))
         (sparql-select
          (s-select
           (append '(?child0 ?uuid0) (Vs '|0|)
                   '(?child1 ?uuid1) (Vs '|1|))
           (s-triples `((?parent mu:uuid ,uuid)
                        ,@(splice-when
                           (and schemes
                                (join
                                 (map (lambda (scheme)
                                        `((?child0 ,(*in-scheme-predicate*) ,scheme)
                                          (?child1 ,(*in-scheme-predicate*) ,scheme)))
                                      schemes))))
                        (?child0 (* ,(*broader-predicate*)) ?parent)
                        (?child1 ,(*broader-predicate*) ?child0)
                        (?child0 mu:uuid ?uuid0)
                        ,@(P '|0|)
                        (?child1 mu:uuid ?uuid1)
                        ,@(P '|1|)))
           order-by: "?child0 ?child1"
           limit: 20)))))

(define (group alists)
  (let ((M (compose delete-duplicates append)))
    (let loop ((alists (cdr alists))
               (alist (car alists))
               (merged '()))
      (if (null? alists) (cons alist merged)
          (match (car alists)
            ((key . vals)
             (if (equal? key (car alist))
                 (loop (cdr alists) (cons key (M (cdr alist) vals)) merged)
                 (loop (cdr alists) (car alists) (cons alist merged)))))))))

(define f
  (match-lambda
    ((node . ((`id . id)
              (`attributes . attributes)
               . children))
     `((data
        . ((id . ,id)
           (type . ,(*json-api-type*))
           ,(append '(attributes) attributes)
           ,@(splice-when 
              (and (not (null? children))
                   `((relationships
                      . ((children 
                          . ((data
                              . ,(list->vector children)))))))))))))))

;; could remove group on recurse... but that seems actually slower
(define (imbricate groups key #!optional (formatter values))
  (let ((group (alist-ref key groups)))
    (if group
        (formatter
         (cons key
               (map (match-lambda
                      ((node . attrs)
                       (if (member node '(id attributes)) ;; ** generalize
                           (cons node attrs)
                           (or (imbricate groups node formatter)
                               (f (cons node attrs))))))
                    group)))
        #f)))

(define (get-node uuid)
  (query-unique-with-vars (node)
     (s-select
      '?node
      (s-triples `((?node mu:uuid ,uuid))))
     node))

(define (descendance schemes uuid 
                     #!optional
                     (properties '((skos:prefLabel description) (skos:altLabel notation)))
                     (lang (*lang*)))
  (hit-hashed-cache
   *cache* (list 'Results schemes uuid lang properties)
   (let ((node (get-node uuid))
         (results (query-statements schemes uuid properties lang)))
     (imbricate (group results) node f))))

(define (get-top-concepts scheme)
  (query-with-vars (node uuid)
     (s-select
      '(?uuid ?node)
      (s-triples `((?node ,(*top-concept-predicate*) ,(or scheme '?scheme))
                   (?node mu:uuid ?uuid))))
     (cons uuid node)))

(define (concept-or-top scheme id)
  (if (equal? id "_top")
      (caar (get-top-concepts scheme))
      id))

(define (get-concept-schemes)
  (query-with-vars (node uuid)
    (s-select
     '(?uuid ?node)
     (s-triples `((?node a ,(*concept-scheme-type*))
                  (?node mu:uuid ?uuid))))
     (cons uuid node)))

(define (scheme-or-default scheme-id)
  (cond ((list? scheme-id)
         #f)
        ((equal? scheme-id "_default")
         (or (*scheme*)
             (*schemes*)
             #f))
        ((equal? scheme-id "_all") #f)
        (else (get-node scheme-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calls

(define (request-properties)
  (let ((pds ((request-vars) 'properties)))
    (and pds (split-properties pds))))

(define (json-ld-context)
  (append `((concept . ,(write-expand-namespace (*concept-type*)))
            (concept-scheme . ,(write-expand-namespace (*concept-scheme-type*)))) 
          (map (match-lambda ((prop-name prop)
                              (cons prop-name (write-expand-namespace prop))))
               (*properties*))))

(define concept-schemes-call
  (rest-call ()
    (let* ((format (or ((request-vars) 'format) (*format*)))
            (concept-schemes (get-concept-schemes)))
       (if (equal? format "json-api")
           (json-api-data
            (map (match-lambda
                   ((id . node)
                    (json-api-object
                     id (write-uri node) "concept-scheme" 
                     links: `((self . ,(conc "/schemes/" id))))))
                 concept-schemes))
           `((data
              . ,(list->vector
                  (map (match-lambda
                         ((id . node)
                          (json-ld-object (write-uri node) "concept-scheme")))
                      concept-schemes)))
             (@context . ,(json-ld-context)))))))

(define top-concepts-call
   (rest-call (scheme-id)
     (let* ((scheme/s (scheme-or-default scheme-id))
            (scheme (if (pair? scheme/s) (car scheme/s) scheme/s))
            (top-concepts (get-top-concepts scheme))
            (format (or ((request-vars) 'format) (*format*))))
       (if (equal? format "json-api")
           `((data 
              . ,(list->vector
                  (map (match-lambda
                         ((id . node)
                          (json-api-object 
                           id (write-uri node) "concept"
                           links: `((self . ,(conc "/schemes/" scheme-id "/"  id))))))
                       top-concepts))))
           (json-ld-object (write-uri scheme) "concept-scheme"
                           `((concepts 
                              . ,(list->vector
                                  (map (match-lambda
                                         ((id . node)
                                          (json-ld-object (write-uri node) "concept")))
                                       top-concepts)))))))))

(define (descendance-call relation inverse?)
  (rest-call (scheme-id id)
    (let* ((scheme/s (scheme-or-default scheme-id))
           (schemes (and scheme/s (if (pair? scheme/s) scheme/s (list scheme/s))))
           (id (concept-or-top (and schemes (car schemes)) id))
           (levels (string->number (or ((request-vars) 'levels) "1"))))
      (descendance schemes id
                   (or (request-properties) (*properties*))
                   (or ((request-vars) 'lang) (*lang*))))))

(define descendants-call (descendance-call 'children #f))

;; (define ancestors-call (descendance-call 'ancestors #t))

(define  (clear-cache-call _)
  (hash-table-clear! *cache*)
  '((cache . "cleared")))

(define-rest-call 'GET '("test") (lambda (b) `((status . "success"))))

(define-rest-call 'GET '("schemes") concept-schemes-call)

(define-rest-call 'GET '("schemes" scheme-id) top-concepts-call)

(define-rest-call 'GET '("schemes" scheme-id id "descendants") descendants-call)

;; (define-rest-call 'GET '("schemes" scheme-id id "ancestors") ancestors-call)

(define-rest-call 'DELETE '("cache") clear-cache-call)

(define-rest-call 'POST '("deltas") (lambda (_) 
                                      (print "received deltas")
                                      "thanks"))

   
(define (test) (descendance "379436c4-08c3-459a-9b75-b094bdfdbaf4"))
;; (define r (query-statements "379436c4-08c3-459a-9b75-b094bdfdbaf4"))
