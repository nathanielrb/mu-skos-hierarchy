(use awful srfi-69 irregex matchable medea)

;(load "s-sparql/sparql.scm")
;(load "s-sparql/rest.scm")
; (load "s-sparql/threads.scm")

(use s-sparql mu-chicken-support spiffy spiffy-request-vars)

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

(define (descendance parent-uuid levels relation #!optional inverse?)
  (match-let (((vars order-by statements) 
               (descendance-query-all-statements '?parent levels inverse?)))
    (let ((results
           (sparql/select
            (select-triples
             vars
             (s-triples
              `((?parent mu:uuid ,parent-uuid)
                ,@statements))
             order-by: (string-join order-by " ")))))
      (imbricate (map (partition-bindings substr-end unnumber) results) 
                 relation))))

(define (unnumber var)
  (let ((svar (->string var)))
    (string->symbol (substring 
                     svar 0
                     (- (string-length svar) 1)))))

(define (properties-variables n)
  (map (lambda (prop)
         (sparql-variable 
          (conc (car prop) n)))
       *properties*))

(define (property-query n)
  (match-lambda 
    ((name predicate)
     (let ((var (sparql-variable (conc name n))))
       (lambda (node)
         (s-optional
          (s-filter
           (s-triple `(,node ,predicate ,var))
           (lang-or-none-filter var (lang)))))))))

(define (properties-query var n)
  (map (lambda (pq) (pq var))
       (map (property-query n)
            *properties*)))

(define (descendance-query-all-statements parent levels #!optional inverse?)
  (let loop ((level levels)
             (vars '())
             (parent parent)
             (order-by '())
             (statements '()))
    (if (= level 0)
        (list vars order-by statements)
        (let ((uuid (sparql-variable (conc "uuid" (->string (- levels level)))))
              (node (sparql-variable (conc "child" (->string (- levels level))))))
          (loop (- level 1)
                (append vars
                        (list node uuid)
                        (properties-variables (- levels level)))
                node
                (append order-by (list (->string node) ))
                (append `(,(if inverse?
                               `(,parent  skos:broader ,node) ;;(or (car-when vars) parent)
                               `(,node skos:broader ,parent))
                          (,node mu:uuid ,uuid)
                          ,@(properties-query node (- levels level)))
                        statements))))))

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
				     (cont (collect (car node) t))))) ; val t)))))
		((equal? (key (caar nodes)) val)
		 (loop (cons (cdar nodes) accum) (cdr nodes)))
		(else (gather-nodes-cps key accum collect
					(lambda (t)
					  (gather-nodes-cps
					   key nodes collect
					   (lambda (u)
					     (cont (append (collect (car node) t) ;;val t)
							   u))))))))))))

(define (gather-nodes key nodes #!optional (collect alist-tree-node))
  (gather-nodes-cps key nodes collect values))

(define (imbricate cs relation)
  (list->vector
   (gather-nodes cdar (map (lambda (c) (map cdr c)) cs) (json-node relation))))

(define (json-node relation)
  (lambda (node tree)
    (let ((id (alist-ref 'uuid node)))
      `(((id . ,(->string id))
         ,@(map (lambda (prop)
                  (cons prop (alist-ref prop node)))
                (map car *properties*))
         ,@(if (null? tree)
               '()
               `((relationships
                  . ((,relation
                      . ((data . ,(list->vector tree)))))))))))))

(define (alist-tree-node val tree)
  (list (cons val tree)))

(define (substr-end s #!optional (len 1))
  (substring s (- (string-length s) len)))

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

;; (define-rest-call ((id) "/hierarchies/:id/descendants")
;;   (lambda ()
;;     `((data 
;;       . ,(descendance id (str->num (or ($ 'levels) "1"))
;;                       'children)))))

;; (define-rest-call ((id) "/hierarchies/:id/ancestors")
;;   (lambda ()
;;     `((data 
;;       . ,(descendance id (str->num (or ($ 'levels) "1"))
;;                       'ancestors #t)))))

;; (define-rest-call "/hierarchies"
;;   (lambda ()
;;     `((data 
;;       . ,(descendance (get-top-concept) (str->num (or ($ 'levels) "1"))
;;                       'ancestors)))))

;; testing

(define hierarchies
   (rest-call ()
     (let (($ (request-vars)))
       `((data 
       . ,(descendance (get-top-concept) (str->num (or ($ 'levels) "1"))
                       'children))))))
(define descendants
  (rest-call (id)
    (let (($ (request-vars)))
      `((data 
         . ,(descendance id (str->num (or ($ 'levels) "1"))
                         'children))))))

(define ancestors
  (rest-call (id)
    (let (($ (request-vars)))
      `((data 
         . ,(descendance id (str->num (or ($ 'levels) "1"))
                         'ancestors #t))))))

(*handlers* `((GET 
               . (((/ "test") . ,(lambda (b) `((status . "success"))))
                  ((/ "hierarchies") . ,hierarchies)
                  ((/ "hierarchies" id "descendants") . ,descendants)
                  ((/ "hierarchies" id "ancestors") . ,ancestors)))))

(vhost-map `((".*" . ,handle-app) ))

(access-log "access.log")
(error-log "error.log")
(debug-log "debug.log")

(start-server port: 4028)

(define (cs levels) (descendance
                     "379436c4-08c3-459a-9b75-b094bdfdbaf4"
                     levels 'children))
