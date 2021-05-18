#lang racket

; Type for respresenting clauses
(struct Var (lit) #:transparent)
(struct And (x y) #:transparent)
(struct Or (x y) #:transparent)
(struct Not (e) #:transparent)
(struct Const (bool) #:transparent)

;; An empty dictionary
(define assign #hash())

; Parses a single clause---
; (list 1 -2) into (Or (Var 1) (Not (Var 2)))
(define (parseSubExp ls)
  (cond [(null? ls) (error "Given an empty sub-expression")]
        [(null? (cdr ls)) (parseNeg (car ls))]
        [else (Or (parseNeg (car ls))
                  (parseSubExp (cdr ls)))]))

; Parses i to (Var i) and -i to (Not (Var i))
(define (parseNeg num)
  (if (< num 0) (Not (Var (* num -1))) (Var num)))

; Parses full list
; Ex. (list '( 1 2) '(-3 2)) into
; (And (Or (Var 1) (Var 2)) (Or (Not (Var 3)) (Var 2)))
(define (parseExp es)
  (cond [(null? es) (error "Given empty list of expressions")]
        [(null? (cdr es)) (parseSubExp (car es))]
        [else (And (parseSubExp (car es))
                   (parseExp (cdr es)))]))


(define (conversion es)
  (cond[(And? es) (append (list (conversion (And-x es))) (conversion (And-y es)))]
       [(Or? es) (append (conversion (Or-x es)) (conversion (Or-y es)))]
       [(Var? es) (list (Var-lit es))]
       [(Not? es) (list (* -1 (Var-lit (Not-e es))))]))
(define (estolist es)
  (define (helper l)
    (cond[(null? l) '()]
         [(if (list? (car l)) (append (list (car l)) (helper (cdr l)))
                          (list l))]))
  (helper (conversion es)))

;;unit propogation

;;getlist of elements
;(define assign null)
(define (dpll l)


  (define (unitlist l)
    (define ans null)
    (define (helper l)
    (cond[(null? l) null]
         [(null? (car l)) null]
         [(if (null? (cdar l)) (begin (set! ans (car l))
                                      ans)
             (helper (cdr l)))]))
    (helper l))

  
(define (condlateral l)
  (define (allelements l)
  (flatten l))
  
  (define (different l)
  (remove-duplicates (allelements l)))

;;if negative exits or not
(define (checking l)
  (define ans null)
  (define (helper l1 l2)
  (define (searchn x l)
      (cond[(null? l) (set! ans (append ans (list x)))]
           [(if (eq? x (-(car l)))  (set! ans (append ans null))
                (searchn x (cdr l)))]))
    (map (lambda(x) (searchn x l2)) l1))
  (helper (different l) (different l))
   ans)

  (define (noot l1 l2)
    (define ans null)
    (define (helper1 x l)
    (define (helper x l)
      (define ans null)
    (define (search x l)
    (cond[(null? l) ans]
         [ (if (eq? x (car l)) (begin (set! ans (append ans (list (car l))))
                                      (search x (cdr l)))
              (search x (cdr l)))]))
      (search x l))
      (if (> (length (helper x l)) 1) (set! ans (append ans (list x)))
          (set! ans (append ans null))))
    (map (lambda(x) (helper1 x l2)) l1)
    ans)
  (cond[(null? (noot (checking l) (allelements l))) null]
       [else (list (car (noot (checking l) (allelements l))))]))
    
     
  (cond[(equal? l null) #t]
       [(equal? l '(())) (begin (set! assign #hash()) #f)]
       [(not (null? (unitlist l))) (Unitpropogation l)]
       [(not (null? (condlateral l))) (Lateral-propogation l)]
       [else (NLU l)]))


(define (Unitpropogation l)
  (define (unitlist l)
    (define ans null)
    (define (helper l)
    (cond[(null? l) null]
         [(null? (car l)) null]
         [(if (null? (cdar l)) (begin (set! ans (car l))
                                      ans)
             (helper (cdr l)))]))
    (helper l))
  (define (helper1 l)
  (define (helper l)
    (define (unitlist l)
    (define ans null)
    (define (helper l)
    (cond[(null? l) #t]
         [(null? (car l)) #f]
         [(if (null? (cdar l)) (begin (set! ans (car l))
                                      ans)
             (helper (cdr l)))]))
    (helper l))
  

  (define (remove-single p l)
    (define ans null) 
  (define (search x l)
  (cond[(null? l ) #f]
       [(equal? x (car l)) #t]
       [else (search x (cdr l))])) 
   (define (removal l)
    (cond[(null? l) ans]
         [(eq? (search p (car l)) #t) (removal (cdr l))]
         [else (begin (set! ans (append ans (list (car l))))
                      (removal (cdr l)))]))
    (removal l))  
                                            
   (define (remove-singlen p l)
    (define (searchn x l)
      (define ans null)
      (define (helper x l)
        (if (null? l) ans
            (cond[(equal? x (-(car l))) (helper x (cdr l))]
                 [(begin (set! ans (append ans (list (car l))))
                         (helper x (cdr l)))])))
      (helper x l))
    (remove-duplicates (map (lambda(x) (searchn p x)) l)))

  (define (assingn x)
  (cond[(< 0 x) (set! assign (dict-set assign x #t))]
       [(> 0 x) (set! assign (dict-set assign (* -1 x) #f))]))

  
 (define (remove-element l)
    (define ans l)
    (define (helper l1 l2)
    (cond[(not (list? l1)) l2]
         [(list? l1)
          (begin (set! ans (remove-single (car l1) ans))
                 (set! ans (remove-singlen (car l1) ans))
                 (assingn (car l1))
                 (if (equal? ans l2) ans (helper (unitlist ans) l2)))]))
    (helper (unitlist l) ans)
   ans)

  (remove-element l))
  (cond[(null? (unitlist l)) l]
       ;[(and (null? (car (helper l))) (eq? (remove-duplicates (length (helper l))) 1)) '(())]
       [(helper l)]))
  (dpll (helper1 l)))


(define (Lateral-propogation l)

  (define (assign1 l)
        (if (< 0  l)  (set! assign (dict-set assign l #t))
        (set! assign  (dict-set assign (* -1 l) #f))))
                  
;(define (allelements l)
;  (flatten l))
;;;;ikkada different and checkingg
;;different
;(define (different l)
;  (remove-duplicates (allelements l)))

;;if negative exits or not
;(define (condlateral l)
;  (define ans null)
;  (define (helper l1 l2)
;  (define (searchn x l)
;      (cond[(null? l) (set! ans (append ans (list x)))]
;           [(if (eq? x (-(car l)))  (set! ans (append ans null))
;                (searchn x (cdr l)))]))
;    (map (lambda(x) (searchn x l2)) l1))
;  (helper (different l) (different l))
; (list (car ans)))
  (define (condlateral l)
  (define (allelements l)
  (flatten l))
  
  (define (different l)
  (remove-duplicates (allelements l)))

;;if negative exits or not
(define (checking l)
  (define ans null)
  (define (helper l1 l2)
  (define (searchn x l)
      (cond[(null? l) (set! ans (append ans (list x)))]
           [(if (eq? x (-(car l)))  (set! ans (append ans null))
                (searchn x (cdr l)))]))
    (map (lambda(x) (searchn x l2)) l1))
  (helper (different l) (different l))
   ans)

  (define (noot l1 l2)
    (define ans null)
    (define (helper1 x l)
    (define (helper x l)
      (define ans null)
    (define (search x l)
    (cond[(null? l) ans]
         [ (if (eq? x (car l)) (begin (set! ans (append ans (list (car l))))
                                      (search x (cdr l)))
              (search x (cdr l)))]))
      (search x l))
      (if (> (length (helper x l)) 1) (set! ans (append ans (list x)))
          (set! ans (append ans null))))
    (map (lambda(x) (helper1 x l2)) l1)
    ans)
  (cond[(null? (noot (checking l) (allelements l))) null]
       [else (list (car (noot (checking l) (allelements l))))]))

(define (remove y l)
  (define ans null)
  (define (search x l1 l2)
    (cond[(null? l1) (set! ans (append ans (list l2)))]
         [(if (eq? x (car l1)) (set! ans (append ans null))
              (search x (cdr l1) l2))]))
  (define (helper l1)
  (cond[(null? y) l]
       [(if (null? l1) ans
       (begin (search (car y) (car l1) (car l1))
               (helper (cdr l1))))]))
  (helper l))

(define (helper1 l)
  (cond[(null? l) null]
       [(and (eq? (length (remove-duplicates l)) 1) (null? (car (remove-duplicates l)))) '(())]
       [(if (null? (condlateral l)) l
            (begin (assign1 (car (condlateral l)))
                   (helper l)))]))

(define (helper l)
  (define ans l)
  (define (helper2 l)
  (cond[(begin (set! ans (remove (condlateral l) l))
               ;(assign (car (checking l)))
               (helper1 ans))]))
  (helper2 l))
  (dpll (helper1 l)))


(define (NLU l)
 (define (helper1 l)
  (define ans null)
  (define (helper l)
    (set! ans (append (list (list (car (car l)))) l)))
  (helper l)
(dpll ans))

  (define (helper2 l)
  (define ans null)
  (define (helper l)
    (set! ans (append (list (list (-(car (car l))))) l)))
  (helper l)
(dpll ans))
  
(if (eq? (helper1 l) #f) (helper2 l)
    (helper1 l)))

















  

                  
























