(use spiffy spiffy-request-vars srfi-69 matchable irregex medea
     s-sparql mu-chicken-support)

(*default-graph* '<http://data.europa.eu/eurostat/ECOICOP>)

(define t '((a . b) (a . c) (b . h) (b . i)(c . j) (c . k)))

(define t2 '((a . (b . ((@attributes B)))) (a . (c  . ((@attributes C)))) (b . (h  . ((@attributes H)))) (b . (i . ((@attributes I))))(c . (j  . ((@attributes J)))) (c . (k . ((@attributes K))))))

(define (query-statements uuid)
  (query-with-vars (child0 child1 uuid0 uuid1)
     (s-select
      '(?child0 ?child1 ?uuid0 ?uuid1)
      (s-triples `((?parent mu:uuid ,uuid)
                   (?child1 skos:broader ?child0)
                   (?child0 mu:uuid ?uuid0)
                   (?child1 mu:uuid ?uuid1)
                   (?child1 (* skos:broader) ?parent)))
      order-by: "?child0 ?child1")
     `((,child0 . ((id . ,uuid0)))
       (,child1 . ((id . ,uuid1))))))
     ;; `(,child0 . ((@attributes
     ;;               . ((id . ,uuid0)))
     ;;              (,child1
     ;;               . ((@attributes
     ;;                   . ((id . ,uuid1)))))))))
(define ref alist-ref)

(define (group pairs)
  (let ((T (lambda (group)
             (cons (caar group) (map cdr group))))
        (U (lambda (head group)
             (cons (car head)
                   (append (cdr head) group)))))
    (let loop ((pairs (cdr pairs)) (groups '()) 
               (group (list (second (car pairs)))) (head (first (car pairs))))
      (if (null? pairs)(cons (U head group) groups)
          (match (car pairs)
            ((a b)
             (if (equal? (car a) (car head)) ;;(car-when (car-when group))))
                 (loop (cdr pairs) groups (cons b group) head)
                 (loop (cdr pairs) 
                       (cons (U head group) groups)
                       (list (second (car pairs)))
                       (first (car pairs))))))))))
        
;; should remove group on recurse, using a loop
;; and add formatter
(define (imbricate groups key #!optional (formatter values))
  (let ((group (alist-ref key groups)))
    (if group
        (formatter
         (cons key
               (map (match-lambda
                      ((node . attrs)
                       (or (imbricate groups node)
                           `(,node . ,attrs))))
                    group)))
        #f)))
              
(define r (query-statements "379436c4-08c3-459a-9b75-b094bdfdbaf4"))
