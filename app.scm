(use spiffy spiffy-request-vars srfi-69 matchable)

(use s-sparql mu-chicken-support)

(*print-queries?* #t)

(define-namespace skos "http://www.w3.org/2004/02/skos/core#")

(define *lang*
  (make-parameter
   (or (get-environment-variable "DEFAULT_LANGUAGE")
       "en")))

(define *data-format*
  (make-parameter
   (or (get-environment-variable "DATA_FORMAT")
       "json-api")))

(define *scheme*
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

(define *cache* (make-hash-table))

(define (get-top-concepts scheme)
  (query-with-vars (node uuid)
     (select-triples
      '(?uuid ?node)
      (s-triples `((?node skos:topConceptOf ,scheme)
                   (?node mu:uuid ?uuid))))
     (cons uuid node)))

(define (get-concept-schemes)
  (query-with-vars (node uuid)
    (select-triples
     '(?uuid ?node)
     (s-triples `((?node a skos:ConceptScheme)
                  (?node mu:uuid ?uuid))))
     (cons uuid node)))

(define (lang-or-none-filter var lang)
  (format #f "lang(~A) = '~A' || lang(~A) = ''" 
          var lang var))

(define (get-node uuid)
  (query-unique-with-vars (node)
     (select-triples
      '?node
      (s-triples `((?node mu:uuid ,uuid))))
     node))

(define (node-properties-id uuid)
  (let ((node (cdr (get-node uuid))))
    (node-properties node)))

(define (node-properties node)
  (hit-property-cache 
   node 'properties
   (if-pair? *properties*
             (let ((properties (properties-query node)))
               properties))))

(define (properties-variables n)
  (map (lambda (prop)
         (sparql-variable 
          (conc (car prop) n)))
       *properties*))

;; currently not OPTIONAL to avoid Virtuoso errors
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
            *properties*)))

(define (descendance-query-all-statements top-parent levels #!optional inverse?)
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
                                      `(,parent  skos:broader ,node)
                                      `(,node skos:broader ,parent))
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

(define (nuull? node)
  (or (null? node) (equal? node '(()))))

(define (gather-nodes-cps key nodes collect cont)
  (if (nuull? nodes)
      (cont '())
      (let* ((node (car nodes))
	     (val (key (car node))))
	(let loop ((accum (list (cdr node)))
		   (nodes (cdr nodes)))
	  (cond ((nuull? nodes)
		 (gather-nodes-cps key accum collect
				   (lambda (t)
				     (cont (collect (car node) t)))))
		((equal? (key (caar nodes)) val)
		 (loop (cons (cdar nodes) accum) (cdr nodes)))
		(else (gather-nodes-cps key accum collect
					(lambda (t)
					  (gather-nodes-cps
					   key nodes collect
					   (lambda (u)
					     (cont (append (collect (car node) t)
							   u))))))))))))

(define (alist-tree-node val tree)
  (list (cons val tree)))

(define (gather-nodes key nodes #!optional (collect alist-tree-node))
  (gather-nodes-cps key nodes collect values))

(define (imbricate cs constructor relation)
  (list->vector
   ;; (gather-nodes cdar (map (lambda (c) (map cdr c)) cs) (json-node relation))))
   (gather-nodes cdadr cs (constructor relation))))

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
    (string->symbol (substring 
                     svar 0
                     (- (string-length svar) 1)))))

(define (substr-end s #!optional (len 1))
  (substring s (- (string-length s) len)))

(define (descendance scheme parent-uuid levels constructor relation #!optional inverse?)
  (match-let (((vars order-by statements) 
               (descendance-query-all-statements '?parent levels inverse?)))
    (print "VARS: " vars)
    (let ((results
           (sparql/select
            (select-triples
             vars
             (s-triples
              `((?parent mu:uuid ,parent-uuid)
                ,@statements))
             order-by: (string-join order-by " ")))))
      (imbricate (map (partition-bindings substr-end unnumber) results) 
                 constructor
                 relation))))

(define (json-api relation)
  (lambda (node tree)
    (let ((id (alist-ref 'uuid node)))
      `(((id . ,(->string id))
         (type . "concept")
         (attributes
          (@id . ,(write-uri (alist-ref 'child node)))
          ,@(map (lambda (prop)
                   (cons prop (alist-ref prop node)))
                 (map car *properties*)))
         ,@(if (null? tree)
               '()
               `((relationships
                  . ((,relation
                      . ((data . ,(list->vector tree)))))))))))))

(define (json-ld relation)
  (lambda (node tree)
    (let ((id (alist-ref 'uuid node)))
      `(((@id . ,(write-uri (alist-ref 'child node)))
         (@type . "concept")
         ,@(map (lambda (prop)
                  (cons prop (alist-ref prop node)))
                (map car *properties*))
         ,@(if (null? tree)
               '()
               `((,relation
                 . ,(list->vector tree)))))))))

(define concept-schemes
   (rest-call ()
     (let* (($ (request-vars))
            (format (or ($ 'format) (*data-format*)))
            (concept-schemes (get-concept-schemes)))
       (if (equal? format "json-api")
           `((data 
              . ,(list->vector
                  (map (match-lambda ((id . node)
                                      `((id . ,id)
                                        (type . "concept-scheme")
                                        (links . ((self . ,(conc "/schemes/" id)))))))
                       concept-schemes))))
           (list->vector
            (map (match-lambda ((id . node)
                                `((@id . ,(write-uri node))
                                  (type . "concept-scheme")
                                  (@context . ,(context)))))
                 concept-schemes))))))                                  

(define (scheme-or-default scheme-id)
  (if (equal? scheme-id "_default")
      (*scheme*)
      (get-node scheme-id)))

(define top-concepts
   (rest-call (scheme-id)
     (let* (($ (request-vars))
           (format (or ($ 'format) (*data-format*)))
           (scheme (scheme-or-default scheme-id))
           (top-concepts (get-top-concepts scheme)))
       (if (equal? format "json-api")
           `((data 
              . ,(list->vector
                  (map (match-lambda ((id . node)
                                      `((id . ,id)
                                        (type . "concept")
                                        (links . ((self . ,(conc "/schemes/" scheme-id "/" id)))))))
                       top-concepts))))
           `((@id . ,(write-uri scheme))
             (@type . "concept-scheme")
             (concepts 
              . ,(list->vector
                  (map (match-lambda ((id . node)
                                      `((@id . ,(write-uri node))
                                        (@type . "concept"))))
                       top-concepts))))))))

(define (concept-or-top scheme id)
  (if (equal? id "_top")
      (caar (get-top-concepts scheme))
      id))

(define (context)
  (append `((concept . ,(expand-namespace 'skos:Concept))
            (concept-scheme . ,(expand-namespace 'skos:ConceptScheme)))
          (map (match-lambda ((prop-name prop)
                              (cons prop-name (expand-namespace prop))))
               *properties*)))

(define (format-constructor format)
  (if (equal? format "json-api")
      json-api
      json-ld))

(define (descendance-call relation inverse?)
  (rest-call (scheme-id id)
    (let* (($ (request-vars))
           (format (or ($ 'format) (*data-format*)))
           (constructor (format-constructor format))
           (scheme (scheme-or-default scheme-id))
           (id (concept-or-top scheme id)))
      (let ((descs (hit-hashed-cache
                    *cache* (list scheme id format ($ 'levels))
                    (descendance scheme id (str->num (or ($ 'levels) "1"))
                                 constructor relation))))
        (if (equal? format "json-api")
            `((data . ,descs))
            `((@id . ,id)
              (@type . "concept")
              (,relation . ,descs)
              (@context  . ,(append
                              `((,relation . ,(expand-namespace 'skos:broader)))
                              (context)))))))))

(define descendants (descendance-call 'children #f))

(define ancestors (descendance-call 'ancestors #t))

(*handlers* `((GET ("test") ,(lambda (b) `((status . "success"))))
              (GET ("schemes") ,concept-schemes)
              (GET ("schemes" scheme-id) ,top-concepts)
              (GET ("schemes" scheme-id id "descendants") ,descendants)
              (GET ("schemes" scheme-id id "ancestors") ,ancestors)))
