#lang racket
(struct node (val ltree rtree) #:transparent)
(struct nulltree () #:transparent)

(define t4 (node 2 
                 (node 1 (nulltree) (nulltree))
                 (node 3 (nulltree) (nulltree))))
(define t6 (node 6 (nulltree) (nulltree)))
(define t7 (node 2  (node 7 (nulltree) (nulltree))
                 (node 9 (nulltree) (nulltree))))
(define t8 (node 8 (node 7 (nulltree) (nulltree))
                 (node 9 (nulltree) (nulltree))))  
(define t10 (node 12 (node 11 (nulltree) (nulltree))
                  (node 13 (nulltree) (nulltree))))
(define t11 (node 16 (node 15 (nulltree) (nulltree))
                  (node 17 (nulltree) (nulltree))))
(define t9 (node 14 t10 t11))
(define t3 (node 10  t8 t9))
(define t5 (node 5 (nulltree) (nulltree)))
(define t2 (node 4  t4 t5))
(define t1 (node 6 t2 t3))
(define t0 (node 5 (node 3 (node 1 (node 0 (nulltree) (nulltree)) (node 2 (nulltree) (nulltree))) (nulltree)) (node 6 (nulltree) (nulltree))))
;;flatten
(define (flatten t)
  (cond [(nulltree? t) '()]
        [else (append (flatten (node-ltree t))
                      (list (node-val t))
                      (flatten (node-rtree t)))]))


;;search
(define (search x t)
  (match t
    [(nulltree) '#f]
    [(node v lt rt)
     (cond [(= x v) #t]
           [(< x v) (search x lt)]
           [else (search x rt)])]))

;(define (insert x t)
 ; (cond[(nulltree? t) (node x (nulltree) (nulltree))]
  ;     [(node? t) (if (< (node-val t) x) (node (node-val t) (node-ltree t) (insert x (node-rtree t)))
   ;                   (node (node-val t) (insert x (node-ltree t)) (node-rtree t)))]))

(define (delete x t)
   (match t
     [(nulltree) t]
     [(node v lt rt) (cond 
                       [(= x v) (join lt rt)]                        
                       [(< x v) (node v (delete x lt) rt)]
                       [else    (node v lt (delete x rt))])]))

(define (deletec x t)
  (match t
    [(nulltree) t]
    [(node v lt rt) (cond[(= x v) (node-ltree t)]
                         [(< x v) (node v (delete x lt) rt)]
                         [(> x v) (node v lt (delete x rt))])]))

; join should satisfy (join lt rt) = (append (flatten lt) (flatten rt))

(define (join t1 t2)
  (define (rightmost t)
    (match t
      [(node v lt (nulltree)) v]
      [(node v lt rt) (rightmost rt)]))

  (define (readjust t)
    (match t
      [(node v lt (nulltree)) lt]
      [(node v lt rt) (readjust rt)]))
          
  (match t1
    [(nulltree) t2]
    [_(node (rightmost t1) (deletec (rightmost t1) t1) t2)]))



;;;;huffman
(struct bnode (ltree rtree) #:transparent)
(struct leaf (val) #:transparent)

;In terms of this structure let us encode the huffman tree for the  ;code

(define example-huffman
  (bnode
   (bnode (leaf 'B) (bnode (leaf 'E) (leaf 'D)))
   (bnode (leaf 'A) (bnode (leaf 'C) (bnode
                                      (bnode (leaf 'H) (leaf 'G))
                                      (leaf 'F))))))


(define (decode t l)
  (define (decode-helper t1 l)
    (match t1
      [(leaf val) (cons val (decode t l))]
      [(bnode lt rt)  (match l
                        ['() (error "Ill formed input string")]
                        [(cons 0 rest) (decode-helper lt rest)]
                        [(cons 1 rest) (decode-helper rt rest)])]))
  (match l
    ['() '()]
    [_ (decode-helper t l)]))


(decode example-huffman '(0 1 1 0 0 0 0 0 0 1 1 1 0 0 0 1 1))
;;;;;


(define (transform t)
  (define (transform-helper t l)
    (match t
      ((leaf v) (list (cons v (reverse l))))
      ((bnode lt rt) (huffmerge (transform-helper lt (cons 0 l))
                                (transform-helper rt (cons 1 l))))))

  (transform-helper t '()))

(define (huffmerge l1 l2)
  (match (cons l1 l2)
    ((cons '() l2) l2)
    ((cons l1 '()) l1)
    ((cons (cons fst1 rest1) (cons fst2 rest2))
     (if (<= (length fst1) (length fst2)) (cons fst1 (huffmerge rest1 l2))
         (cons fst2 (huffmerge l1 rest2))))))


(define tbl (transform example-huffman))

(define (encode l)
  
  (define (lookup sym tbl)
    (match tbl
      ['() (error "Bad symbol")]
      [(cons (cons sym1 code) rest) (if (eq? sym sym1) code
                                        (lookup sym rest))]))

  (define (f x y) (append (lookup x tbl) y))
  (foldr f '() l))
  
(define initial-list (list (cons 'H 1) (cons 'G 1)
                           (cons 'F 1) (cons 'E 1)
                           (cons 'D 1) (cons 'C 1)
                           (cons 'B 3) (cons 'A 9)))

(define (convert-initial l)
  (map (lambda (x) (cons (leaf (car x)) (cdr x))) l))
;;;;;;;;;;;;

(define (combine p1 p2)
  (match (cons p1 p2)
    [(cons (cons t1 v1) (cons t2 v2))
     (cons (bnode t1 t2) (+ v1 v2))]))

(define (insert p l)
  
  (append (takef l  (lambda (x) (< (cdr x) (cdr p))))
          (list p)
          (dropf l (lambda (x) (< (cdr x) (cdr p))))))

(define (combine-and-insert l)
  (match l
    [(cons x (cons y rest))
     (insert  (combine x y) rest)]))

(define (singleton? l)
  (null? (cdr l)))
(define (until p f l)
  (cond [(p l) l]
        [else (until p f (f l))]))

(define huffman-tree
  (car (until singleton? combine-and-insert (convert-initial initial-list))))