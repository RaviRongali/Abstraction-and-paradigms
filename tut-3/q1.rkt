#lang racket
(struct gnode(val lst) #:transparent)
(define g2 (gnode 1 (list (gnode 2 (list (gnode 3 '())
                                         (gnode 5 '())))
                          (gnode 6 (list (gnode 7 '())
                                         (gnode 8 '()))))))

(define (at-level t n)
  (cond[(= n 1) (list (gnode-val t))]
       [(null? (gnode-lst t)) '()]
       [else (append* (map (lambda(x) (at-level x (- n 1))) (gnode-lst t)))]))

;;q2
(struct bnode (ltree val rtree) #:transparent)
(struct leaf (val) #:transparent)
(define t1
 (bnode (bnode (bnode (leaf 2) 5 (leaf 3))
             10
             (leaf 4)
             )
       10
       (bnode (leaf 5)
             15
             (leaf 6)
             )
 ))

(define (diameter b)
  (cond[(leaf? b) 1]
       [(bnode? b) (max (diameter (bnode-ltree b)) (diameter (bnode-rtree b)) (+ (height (bnode-ltree b)) (height (bnode-rtree b)) 1))]))
(define (height b)
  (cond[(leaf? b) 1]
       [(bnode? b) (+ 1 (max (height (bnode-ltree b))) (max (height (bnode-rtree b))))]))


;;q3
(struct node (val ltree rtree) #:transparent)
(struct nulltree () #:transparent)

(define n1 (node 4 (node 4 (node 2 '() '())
                         (node 3 '() '()))
                 (node 6 (node 5 '() '())
                       (node 7 '() '()))))

(define (list-within bst lb ub)
  (cond[(null? bst) '()]
       [(< (node-val bst) lb) (list-within (node-rtree bst) lb ub)]
       [(> (node-val bst) ub) (list-within (node-ltree bst) lb ub)]
       [else (append (list (node-val bst)) (list-within (node-ltree bst) lb (node-val bst))
                      (list-within (node-rtree bst) (+ 1 lb) ub))]))


;;q5

























       