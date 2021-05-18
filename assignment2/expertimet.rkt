

#lang racket
(require "declarations.rkt")
(require "utilities.rkt")


(define (buildNullable t)
  (define (helper t)
    (cond[(Epsilon? t) (list (list (Epsilon-n t) #t))]
         [(Literal? t) (list (list (Literal-n t) #f))]
         [(Or? t) (append  (list (list (Or-n t) #f)) (helper (Or-t1 t)) (helper(Or-t2 t)))]
         [(Then? t) (append (list  (list (Then-n t) #f)) (helper (Then-t1 t)) (helper(Then-t2 t)))]
         [(Star? t) (append (list (list (Star-n t) #t)) (helper (Star-t t)))]))
  (helper t))
(define (nullable? t)
  (cond[(Or? t) (or (nullable? (Or-t1 t)) (nullable? (Or-t2 t)))]
       [(Then? t) (and (nullable? (Then-t1 t)) (nullable? (Then-t2 t)))]
       [(Star? t) #t]
       [(Epsilon? t) #t]
       [else #f]))
;;;;;;;;
(define (buildFirst t)
  (define (helper t)
    (cond[(Epsilon? t) null]
         [(Literal? t) (list (Literal-n t))]
         [(Then? t) (if (nullable? (Then-t1 t)) (append (helper (Then-t1 t)) (helper (Then-t2 t)))
                        (helper (Then-t1 t)))]
         [(Or? t) (append (helper (Or-t1 t)) (helper (Or-t2 t)))]
         [(Star? t) (helper (Star-t t))]))
  (cond[(Epsilon? t) (list (list (Epsilon-n t)))]
       [(Literal? t) (list (append (list (Literal-n t)) (helper t)))]
       [(Then? t) (append (list (append (list (Then-n t))(helper t))) (buildFirst (Then-t1 t)) (buildFirst (Then-t2 t)))]
       [(Or? t) (append (list (append (list (Or-n t)) (helper t))) (buildFirst (Or-t1 t)) (buildFirst (Or-t2 t)))]
   [(Star? t) (append (list (append (list (Star-n t)) (helper t))) (buildFirst (Star-t t)))]))
;;;;;;;;;
(define (buildLast t)
  (define (helper t)
    (cond[(Epsilon? t) null]
         [(Literal? t) (list (Literal-n t))]
         [(Then? t) (if (nullable? (Then-t2 t)) (append (helper (Then-t1 t)) (helper (Then-t2 t)))
                        (helper (Then-t2 t)))]
         [(Or? t) (append (helper (Or-t1 t)) (helper (Or-t2 t)))]
         [(Star? t) (helper (Star-t t))]))
  (cond[(Epsilon? t) (list (list (Epsilon-n t)))]
       [(Literal? t) (list (append (list (Literal-n t)) (helper t)))]
       [(Then? t) (append (list (append (list (Then-n t))(helper t))) (buildLast (Then-t1 t)) (buildLast (Then-t2 t)))]
       [(Or? t) (append (list (append (list (Or-n t)) (helper t))) (buildLast (Or-t1 t)) (buildLast (Or-t2 t)))]
       [(Star? t) (append (list (append (list (Star-n t)) (helper t))) (buildLast (Star-t t)))]))
(define (append1 x y)
    (map (lambda (x) (append x y)) x))
;;;;;
(define (buildFollow t)
(define (Followpos1 t)
  (define (helperf t)
    (cond[(Epsilon? t) null]
         [(Literal? t) (list (Literal-n t))]
         [(Then? t) (if (nullable? (Then-t1 t)) (append (helperf (Then-t1 t)) (helperf (Then-t2 t)))
                        (helperf (Then-t1 t)))]
         [(Or? t) (append (helperf (Or-t1 t)) (helperf (Or-t2 t)))]
         [(Star? t) (helperf (Star-t t))]))
  (define (helperl t)
    (cond[(Epsilon? t) null]
         [(Literal? t) (list (list (Literal-n t)))]
         [(Then? t) (if (nullable? (Then-t2 t)) (append (helperl (Then-t1 t)) (helperl (Then-t2 t)))
                        (helperl (Then-t2 t)))]
         [(Or? t) (append (helperl (Or-t1 t)) (helperl (Or-t2 t)))]
         [(Star? t) (helperl (Star-t t))]))
  (cond[(Literal? t )  null]
       [(Epsilon? t) null]
       [(Then? t) (append (append1 (helperl (Then-t1 t)) (helperf (Then-t2 t))) (Followpos1 (Then-t1 t)) (Followpos1 (Then-t2 t)))]
       [(Or? t) (append (Followpos1 (Or-t1 t)) (Followpos1 ( Or-t2 t)))]
       [(Star? t) (append (append1  (helperl t) (helperf t)) (Followpos1 (Star-t t)))]))

(define (search x l)
  (cond [(null? l) null]
    [(eq? (car (car l)) x)  (append  (cdr (car l)) (search x (cdr l)))]
        [(search x (cdr l))]))
  

  (define (allfirst t)
    (define (helper t)
    (if (null? t) null
    (append (list (car (car t))) (allfirst (cdr t)))))
    (remove-duplicates (helper t)))
(define (ass p)
  (cond[(null? p)  null]
       [(append  (list (append* (list (list (car (car p))) (asc (cdr (car p)))))) (ass (cdr p)))]))

(define (asc l)
(define (insertToSortedList element lst)
  (cond
    [(or (empty? lst) (<= element (first lst))) (cons element lst)]
    [else (cons (first lst) (insertToSortedList element (rest lst)))]))

(define (sort-demo1 lst)
  (cond
    [(empty? lst) empty]
    [else (insertToSortedList (first lst) (sort-demo1 (rest lst)))]))
  (sort-demo1 l))
  
  (define (helper x y )
   (cond[(null? y) null]
        [(append (list (append (list (car y)) (search (car y) x))) (helper x (cdr y)))]))
  (map(lambda(x) (append (list (car x)) (remove-duplicates (cdr x))))
  (ass (helper (Followpos1 t) (allfirst (Followpos1 t))))))

;;;;;;;;;;

(define (buildGraph reg)
  (define t (maketree reg))
  (buildgraph t))
  (define (buildgraph x)
;greennode
(define (greennode t)
  (define (first t)
    (cond[(Epsilon? t) null]
         [(Literal? t) (list (Literal-n t))]
         [(Then? t) (if (nullable? (Then-t1 t)) (append (first (Then-t1 t)) (first (Then-t2 t)))
                        (first (Then-t1 t)))]
         [(Or? t)  (append (first (Or-t1 t)) (first (Or-t2 t)))]
         [(Star? t) (first (Star-t t))])) 
  (first t))


;symbols and values
(define (node-sy-val t)
    (cond[(Epsilon? t) null]
         [(Literal? t) (if (equal? (Literal-c t) "#") null (list (list (Literal-c t) (Literal-n t))))]
         [(Then? t) (append (node-sy-val (Then-t1 t)) (node-sy-val (Then-t2 t)))]
         [(Or? t)  (append (node-sy-val (Or-t1 t)) (node-sy-val (Or-t2 t)))]
         [(Star? t) (node-sy-val (Star-t t))]))
;search first element and output cdrs

;all symbols
(define (node-sym t)
(define (node-sy t)
    (cond[(Epsilon? t) null]
         [(Literal? t) (list (Literal-c t))]
         [(Then? t) (append (node-sy (Then-t1 t)) (node-sy (Then-t2 t)))]
         [(Or? t)  (append (node-sy (Or-t1 t)) (node-sy (Or-t2 t)))]
         [(Star? t) (node-sy (Star-t t))]))
  (remove-duplicates (node-sy t)))
;;all first elements of list
(define (allfirst t)
    (define (helper t)
    (if (null? t) null
    (append (list (car (car t))) (allfirst (cdr t)))))
    (remove-duplicates (helper t)))


(define (requirelist t)
  (define (helper t)
  (define (search t l)
  (cond [(null? l) null]
    [(equal? (car (cdr (car l))) t)  (append  (list (car l)) (search t (cdr l)))]
        [(search t (cdr l))]))
  (cond[(null? t) null]
       [(append  (search (car t) (node-sy-val x)) (helper (cdr t)))]))
  (helper t)) 
  (define (Start1 t)
    (define (helper1 x y )
   (cond[(null? y) null]
        [(append (list (append (list (car y)) (search1 x (car y) ))) (helper1 x (cdr y)))]))

(define (search1 l x)
  (cond [(null?  l) null]
        [(if (equal? (car (car l)) x)  (append (cdr (car l)) (search1 (cdr l) x))
         (search1 (cdr l) x))]))
 
    (helper1 (requirelist t) (allfirst (requirelist t))))
;equality of lists
(define (found? l)
  (define (helper p)
  (cond [(null? p) #f]
        [(equal? l (car p)) #t]
        [(helper (cdr p))]
        [else #f]))
  (helper global))
      


(define (final x1)
 (define  (helper1 x1)
  (cond[(null? x1) null]
       [(append (list (helper (buildFollow x) (cdr (car x1)))) (helper1 (cdr x1)))]))
(define (helper x y )
   (cond[(null? y) null]
        [(append (search1 x (car y)) (helper x (cdr y)))]))
(define (search1 x l)
  (cond [(null?  x) null]
        [(if (equal? (car (car x)) l)  (append (cdr (car x)) (search1 (cdr x) l))
         (search1 (cdr x) l))]))
  (helper1 (Start1 x1))) 

 (define  (helper1 t)
  (cond[(null? x) null]
       [(append (list (helper (buildFollow x) (cadr (car t)))) (helper1 (cdr t)))]))

(define (helper x y )
   (cond[(null? y) null]
        [(append (list (search1 (car y) x))) (helper x (cdr y))]))

(define (search1 l x)
  (cond [(null?  l) null]
        [(if (equal? (car (car l)) x)  (append (cdr (car l)) (search1 (cdr l) x))
         (search1 (cdr l) x))]))
;;append all the nodes of graph
(define (final1 x1)
 (define  (helper1 x1)
  (cond[(null? x1) null]
       [(append (list (append (list (car (car x1))) (helper (buildFollow x) (cdr (car x1))))) (helper1 (cdr x1)))]))
(define (helper x y )
   (cond[(null? y) null]
        [(append (search1 x (car y)) (helper x (cdr y)))]))
(define (search1 x l)
  (cond [(null?  x) null]
        [(if (equal? (car (car x)) l)  (append (cdr (car x)) (search1 (cdr x) l))
         (search1 (cdr x) l))]))
  (helper1 (Start1 x1)))
(define (setfound y)
  (cond [(null? y) (set! global (append global null))]
        [(if(equal? (found? (cdr (car y))) #f) (begin (set! global (append global (list (cdr (car y))))) (setfound (cdr y)))
           (setfound (cdr y)))]))
(define  global null)  
(define trans null)



(define global2 null)
(define (trans4 x)
  (define (found1? l)
  (define (helper p)
  (cond [(null? p) #f]
        [(equal? l (car p)) #t]
        [(helper (cdr p))]
        [else #f]))
  (helper global2))
  (define (helper x)
  (define global1 null)
   ; [(null? x) (set! global (append global (list null)))]
  (define (setfound p)
  (cond [(null? p) null];(set! global (append global  null))]
        [(if (equal? (found1? (cdr (car p))) #f) (begin  ;(set! global (append global (list (cdr (car p)))))
                                                        (set! global1 (append global1 (list (cdr (car p)))))
                                                        (setfound (cdr p)))
           (setfound (cdr p)))]))
  (setfound (final1 x))
  (set! global2 (append global2 (list x)))
   global1)
  (map (lambda(x) (trans4 x)) (helper x))
 (remove-duplicates global2))
  
(define (trans1 t)
  (define (final1 x1)
 (define  (helper1 x1)
  (cond[(null? x1) null]
       [(append (list (append (list (car (car x1))) (helper (buildFollow x) (cdr (car x1))))) (helper1 (cdr x1)))]))
(define (helper x y )
   (cond[(null? y) null]
        [(append (search1 x (car y)) (helper x (cdr y)))]))
(define (search1 x l)
  (cond [(null?  x) null]
        [(if (equal? (car (car x)) l)  (append (cdr (car x)) (search1 (cdr x) l))
         (search1 (cdr x) l))]))
  (helper1 (Start1 x1)))
  (define (helper y)
  (cond [(null?  y) (set! trans (append trans null))]
        [(if (equal? (found? (cdr (car y))) #f) (begin (set! trans (append trans (list (list t (list (car (car y)))  (cdr (car y))))))  (helper (cdr y)))
           (helper (cdr y)))]))
  (helper (final1 t)))


(define (transfinal x)
(define (alltrans x)
  (define (listoftrans p)
 (define (helper p)
 (cond[(null? p) (trans1 null)]
   [(begin (trans1 (car p)) (listoftrans (cdr p)))]))
  (helper p)
  trans)
  (define (helper x)
  (cond[(null? x) null]
       [(append (helper (cdr x)) (list (Trans (car (car x)) (car (car (cdr (car x)))) (car (cdr (cdr (car x))))))
                )]))
  (helper (listoftrans x)))
(remove-duplicates (alltrans (trans4 (greennode x)))))

    
;(define (rednods x)
 (define (rednodes x)
 (define (node-sy-val1 t)
    (cond[(Epsilon? t) null]
         [(Literal? t) (if (equal? (Literal-c t) "#") (Literal-n t) null )]
         [(Then? t) (append (node-sy-val1 (Then-t1 t)) (node-sy-val1 (Then-t2 t)))]
         [(Or? t)  (append (node-sy-val1 (Or-t1 t)) (node-sy-val1 (Or-t2 t)))]
         [(Star? t) (node-sy-val1 (Star-t t))]))
(define (search2 l x)
  (cond [(null?  l) null]
        [(if (equal? (car (reverse (car l))) x)  (append  (list (car l)) (search2 (cdr l) x))
         (search2 (cdr l) x))]))
  (search2  (trans4 (greennode x)) (node-sy-val1 x)))
(define (nodes x)
  (trans4 (greennode x)))
 
    (Graph (greennode x) (nodes x) (transfinal x) (rednodes x) (node-sym x)))



     