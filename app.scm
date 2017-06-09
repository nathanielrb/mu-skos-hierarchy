(use spiffy spiffy-request-vars srfi-69 matchable irregex
     s-sparql mu-chicken-support)

(*print-queries?* #t)

(define *lang*
  (config-param "DEFAULT_LANGUAGE" "en"))

(define *format*
  (config-param "DATA_FORMAT" "json-api"))

(define *batch-levels*
  (config-param "BATCH_LEVELS" 6))

(define *scheme*
  (config-param "CONCEPT_SCHEME" #f read-uri))

(define *property-definitions*
  (config-param "INCLUDED_PROPERTIES" ""))

(define *concept-scheme-type*
  (config-param "CONCEPT_SCHEME_TYPE" "skos:ConceptScheme" read-uri))

(define *concept-type* (config-param "CONCEPT_TYPE" "skos:Concept" read-uri))

(define *top-concept-predicate*
  (config-param "TOP_CONCEPT_PREDICATE" "skos:topConceptOf" read-uri))

(define *broader-predicate*
  (config-param "BROADER_PREDICATE" "skos:broader" read-uri))

(define (split-properties property-definitions)
  (map (lambda (str) 
         (map string->symbol (string-split str "="))) 
       (string-split property-definitions ",")))

(define *properties*
  (make-parameter
   (split-properties (*property-definitions*))))

(define *cache* (make-hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPARQL queries

(define (get-top-concepts scheme)
  (query-with-vars (node uuid)
     (s-select
      '(?uuid ?node)
      (s-triples `((?node ,(*top-concept-predicate*) ,scheme)
                   (?node mu:uuid ?uuid))))
     (cons uuid node)))

(define (get-concept-schemes)
  (query-with-vars (node uuid)
    (s-select
     '(?uuid ?node)
     (s-triples `((?node a ,(*concept-scheme-type*))
                  (?node mu:uuid ?uuid))))
     (cons uuid node)))

(define (lang-or-none-filter var lang)
  (format #f "lang(~A) = '~A' || lang(~A) = ''" 
          var lang var))

(define (get-node uuid)
  (query-unique-with-vars (node)
     (s-select
      '?node
      (s-triples `((?node mu:uuid ,uuid))))
     node))

(define (properties-variables n)
  (map (lambda (prop)
         (sparql-variable 
          (conc (car prop) n)))
       (*properties*)))

;; could be OPTIONAL, currently not to avoid Virtuoso errors
(define (property-query n)
  (match-lambda 
    ((name predicate)
     (let ((var (sparql-variable (conc name n))))
       (lambda (node)
         (s-filter
          (s-triple `(,node ,predicate ,var))
          (lang-or-none-filter var (*lang*))))))))

(define (properties-query var n)
  (map (lambda (pq) (pq var))
       (map (property-query n)
            (*properties*))))

(define (descendance-query-all-statements top-parent scheme levels #!optional inverse?)
  (let loop ((level levels)
             (vars '())
             (order-by '())
             (statements ""))
    (if (= level 0)
        (list  vars order-by (list statements))
        (let* ((uuid (sparql-variable (conc "uuid" (->string level))))
               (node (sparql-variable (conc "child" (->string level))))
               (parent (if (equal? level 1)
                           top-parent
                           (sparql-variable (conc "child" (->string (- level 1))))))
               (new-statements `(,(if inverse?
                                      `(,parent  ,(*broader-predicate*) ,node)
                                      `(,node ,(*broader-predicate*) ,parent))
                                 ,@(splice-when scheme `((,node skos:inScheme ,scheme)))
                                 (,node mu:uuid ,uuid)
                                 ,@(properties-query node level))))
          (loop (- level 1)
                (append (list node uuid)
                        (properties-variables level)
                        vars)
                (append (list (->string node)) order-by)
                (if (equal? level 1)
                    (conc (s-triples new-statements) statements)
                    (s-optional (conc (s-triples new-statements) statements))))))))

(define (descendance-query scheme parent-uuid levels inverse?)
  (hit-hashed-cache
   *cache* (list 'Query scheme parent-uuid levels (*lang*) (*properties*))
   (match-let (((vars order-by statements) 
                (descendance-query-all-statements '?parent scheme levels inverse?)))
     (sparql/select
      (s-select
       vars
       (s-triples
        `((?parent mu:uuid ,parent-uuid)
          ,@statements))
       order-by: (string-join order-by " "))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building Tree Structure

(define (nuull? node)
  (or (null? node) (equal? node '(()))))

(define (gather-nodes-cps key nodes collect cont #!optional (level 0))
  (if (nuull? nodes)
      (cont '())
      (let* ((node (car nodes))
	     (val (key (car node))))
	(let loop ((accum (list (cdr node)))
		   (nodes (cdr nodes)))
	  (cond ((nuull? nodes)
		 (gather-nodes-cps key accum collect
				   (lambda (t)
				     (cont (collect (car node) t level)))
                                   (+ level 1)))
		((equal? (key (caar nodes)) val)
		 (loop (cons (cdar nodes) accum) (cdr nodes)))
		(else (gather-nodes-cps key accum collect
					(lambda (t)
					  (gather-nodes-cps
					   key nodes collect
					   (lambda (u)
					     (cont (append (collect (car node) t level)
							   u)))
                                           level))
                                        (+ level 1))))))))

(define (alist-tree-node val tree)
  (list (cons val tree)))

(define (gather-nodes key nodes #!optional (collect alist-tree-node))
  (gather-nodes-cps key nodes collect values))

(define (imbricate cs constructor relation batch-continuation)
  (list->vector
   (gather-nodes cdadr cs (constructor relation batch-continuation))))

(define (partition-bindings key proc)
  (lambda (bindings)
    (let loop ((bindings bindings))
      (if (null? bindings)
          '()
          (let* ((first-binding (car bindings))
                 (first-var (->string (car first-binding)))
                 (n (key first-var)))
            (let-values (((part rest)
                          (partition (lambda (x)
                                       (let ((s (->string (car x))))
                                         (equal? n (key s))))
                                     bindings)))
              (append (list
                       (map (lambda (pair)
                              (cons (proc (car pair))
                                    (cdr pair)))
                            part))
                      (loop rest))))))))

(define (unnumber var)
  (let ((svar (->string var)))
    (string->symbol (irregex-replace "[0-9]+$" svar ""))))

(define (substr-end s #!optional (len 1))
  (substring s (- (string-length s) len)))

(define (descendance scheme parent-uuid levels relation #!optional inverse?)
  (let ((levels-remaining (max 0 (- levels (*batch-levels*))))
        (levels-now (min (*batch-levels*) levels)))
    (hit-hashed-cache
     *cache* (list 'Results scheme parent-uuid (*format*) levels (*lang*) (*properties*))
     (let ((results (descendance-query scheme parent-uuid levels-now inverse?)))
       (imbricate (map (partition-bindings substr-end unnumber) results) 
                  (format-constructor)
                  relation
                  (and (> levels-remaining 0)
                      (lambda (id)
                        (descendance scheme id levels-remaining relation inverse?))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Format

(define (properties-object node)
  (map (lambda (prop)
         (cons prop (alist-ref prop node)))
       (map car (*properties*))))

(define (json-api relation continue)
  (lambda (node tree level)
    (let ((id (->string (alist-ref 'uuid node))))
      (list
       (json-api-object 
        id "concept"
        attributes: (properties-object node)
        relationships: (if (and (= level (- (*batch-levels*) 1))
                                (null? tree) continue)
                           (json-api-relationship 
                            relation (json-api-data (continue id)))
                           (json-api-relationship
                            relation (json-api-data tree))))))))

(define (json-ld relation continue)
  (lambda (node tree)
    (list
     (json-ld-object 
      (write-uri (alist-ref 'child node))
      "concept" 
      (append (properties-object node)
              (if (and (null? tree) continue)
                  (jkv-when relation (continue (alist-ref 'uuid node)))
                  (jkv-when relation (list->vector tree))))))))

(define (format-constructor)
  (if (equal? (*format*) "json-api")
      json-api
      json-ld))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calls

(define (concept-or-top scheme id)
  (if (equal? id "_top")
      (caar (get-top-concepts scheme))
      id))

(define (scheme-or-default scheme-id)
  (cond ((equal? scheme-id "_default")
         (*scheme*))
        ((equal? scheme-id "_all") #f)
        (else (get-node scheme-id))))

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
                     id "concept-scheme" 
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
     (let* ((scheme (scheme-or-default scheme-id))
            (top-concepts (get-top-concepts scheme))
            (format (or ((request-vars) 'format) (*format*))))
       (if (equal? format "json-api")
           `((data 
              . ,(list->vector
                  (map (match-lambda
                         ((id . node)
                          (json-api-object 
                           id "concept"
                           links: `((self . ,(conc "/schemes/" scheme-id "/" id))))))
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
    (let* ((scheme (scheme-or-default scheme-id))
           (id (concept-or-top scheme id))
           (levels (string->number (or ((request-vars) 'levels) "1"))))
      (parameterize ((*properties* (or (request-properties) (*properties*)))
                     (*format* (or ((request-vars) 'format) (*format*)))
                     (*lang* (or ((request-vars) 'lang) (*lang*))))
        (let ((descs (descendance scheme id levels relation)))
          (if (equal? (*format*) "json-api")
              `((data . ,descs))
              (json-ld-object (write-uri
                               (get-node id))
                              "concepts"
                              `((relation . ,descs))
                              context: (append
                                        `((,relation 
                                           . ,(write-expand-namespace 
                                               (*broader-predicate*))))
                                        (json-ld-context)))))))))

(define descendants-call (descendance-call 'children #f))

(define ancestors-call (descendance-call 'ancestors #t))

(*handlers* `((GET ("test") ,(lambda (b) `((status . "success"))))
              (GET ("schemes") ,concept-schemes-call)
              (GET ("schemes" scheme-id) ,top-concepts-call)
              (GET ("schemes" scheme-id id "descendants") ,descendants-call)
              (GET ("schemes" scheme-id id "ancestors") ,ancestors-call)))
