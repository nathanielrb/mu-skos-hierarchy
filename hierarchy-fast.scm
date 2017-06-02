(use awful srfi-69 irregex matchable medea)

;(load "s-sparql/sparql.scm")
;(load "s-sparql/rest.scm")
; (load "s-sparql/threads.scm")

(use s-sparql)
(use s-sparql-rest)
(require-extension sort-combinators)

(development-mode? #f)

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

(define (get-top-concept)
  (query-unique-with-vars (node uuid)
     (select-triples
      '?uuid
      (s-triples `((?node skos:topConceptOf ,(scheme))
                   (?node mu:uuid ?uuid))))
     uuid))
         
(define (lang-or-none-filter var lang)
  (format #f "lang(~A) = '~A' || lang(~A) = ''" 
          var lang var))

(define (get-node uuid)
  (query-unique-with-vars (node)
     (select-triples
      '(?node ?uuid)
      (s-triples `((?node mu:uuid ,uuid))))
     (cons uuid node)))

(define (node-properties-id uuid)
  (let ((node (cdr (get-node uuid))))
    (node-properties node)))

(define (node-properties node)
  (hit-property-cache 
   node 'properties
   (if-pair? *properties*
             (let ((properties (properties-query node)))
               properties))))

(define (descendance scheme parent-uuid levels)
  (match-let (((all-vars . statements) 
               (descendance-query-all-statements scheme '?parent levels)))
    (let ((vars (drop-alternating (reverse all-vars)))
          (results
           (sparql/select
            (select-triples
             all-vars
             (s-triples
              `((?parent mu:uuid ,parent-uuid)
                ,@statements))))))
      (imbricate (map partition-bindings results)) )))

(define (drop-alternating l)
  (if (or (null? l) (= (length l) 1))
      l
      (cons (car l) (drop-alternating (cddr l)))))

(define (property-query n)
  (match-lambda 
    ((name predicate)
     (let ((var (sparql-variable (conc name n))))
       (lambda (node)
;         (cons var
               (s-optional
                (s-filter
                 (s-triple `(,node ,predicate ,var))
                 (lang-or-none-filter var (lang)))))))))

(define (properties-query var n)
  (map (lambda (pq) (pq var))
       (map (property-query n)
            *properties*)))

(define (properties-variables n)
  (map (lambda (prop)
         (sparql-variable 
          (conc (car prop) n)))
       *properties*))

(define (descendance-query-all-statements scheme parent levels)
  (let loop ((level levels)
             (vars '())
             (statements '()))
    (if (= level 0)
        (cons vars statements)
        (let ((uuid (sparql-variable (conc "uuid" (->string (- levels level)))))
              (node (sparql-variable (conc "child" (->string (- levels level))))))
          (loop (- level 1)
                ;(cons node (cons uuid vars))
                (append vars
                        (list node uuid)
                        (properties-variables (- levels level)))
                (append `((,node skos:broader ,(or (car-when vars) parent))
                          (,node mu:uuid ,uuid)
                          ,@(properties-query node level))
                        
                        statements))))))

(define (nuull? node)
  (or (null? node) (equal? node '(()))))

(define (gather-nodes-cps key nodes collect cont)
  (if (nuull? nodes)
      (cont '())
      (let* ((node (car nodes))
	     (val (car node)))
	(let loop ((accum (list (cdr node)))
		   (nodes (cdr nodes)))
	  (cond ((nuull? nodes)
		 (gather-nodes-cps key accum collect
				   (lambda (t)
				     (cont (collect val t)))))
		((equal? (key (car nodes)) val)
		 (loop (cons (cdar nodes) accum) (cdr nodes)))
		(else (gather-nodes-cps key accum collect
					(lambda (t)
					  (gather-nodes-cps
					   key nodes collect
					   (lambda (u)
					     (cont (append (collect val t)
							   u))))))))))))

(define (gather-nodes key nodes #!optional (collect alist-tree-node))
  (gather-nodes-cps key nodes collect values))

(define (imbricate cs)
(list->vector
 (gather-nodes cdar (map (lambda (c) (map cdr c)) cs) json-node)))

(define (json-node node tree)
  (print node)
  (let ((id (cdar node))
        (name (cdr-when (cdr-when node))))
    `(((id . ,(->string id))
       (name . ,name)
     ;;
       (relationships . ((data . ,(list->vector tree))))))))

(define (alist-tree-node val tree)
  (list (cons val tree)))


(define (partition-variables vars)
  (if (null? vars)
      '()
      (let* ((first-var (->string (car vars)))
             (n (substring first-var (- (string-length first-var) 1))))
        (let-values (((part rest)
                      (partition (lambda (x)
                                   (let ((s (->string x)))
                                     (equal? n (substring s (- (string-length s) 1)))))
                                 vars)))
          (append (list part) (partition-variables rest))))))

(define (partition-bindings bindings)
  (if (null? bindings)
      '()
      (let* ((first-binding (car bindings))
             (first-var (->string (car first-binding)))
             (n (substring first-var (- (string-length first-var) 1))))
        (let-values (((part rest)
                      (partition (lambda (x)
                                   (let ((s (->string (car x))))
                                     (equal? n (substring s (- (string-length s) 1)))))
                                 bindings)))
          (append (list part) (partition-bindings rest))))))

(define (cs levels) (descendance
            '<http://data.europa.eu/eurostat/id/taxonomy/ECOICOP>
            "379436c4-08c3-459a-9b75-b094bdfdbaf4"
            levels))

(define-rest-call "/hierarchies"
  (lambda ()
    `((data 
       . ,(tree get-descendants (get-top-concept) (str->num ($ 'levels)))))))

(define-rest-call ((id) "/hierarchies/:id/descendants")
  (lambda ()
    `((data 
       . ,(descendance (scheme) id (str->num (or ($ 'levels) "1")))))))

(define-rest-call ((id) "/hierarchies/:id/ancestors")
  (lambda ()
    (reverse-tree 
     id (str->num ($ 'levels)))))
