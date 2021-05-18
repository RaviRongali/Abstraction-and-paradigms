#lang racket
(define (power a n)
  (cond[(= n 0) 1]
       [ ( * a (power  a (- n 1)))]))
(define (reverse l)
  (cond[(null? l) l]
       [else (append (reverse (cdr l)) (list (car l)))]))
 (define (ravi1 y)
    (cond[(= y 0) '(0)]
         [(= y 1) '(1)]
         [else ( append (ravi1 (quotient y 2)) (list (remainder y 2)))]))

;q1
(define (b2u n a )
 (cond[(= n 0) 0]
      [else ( + ( * (power 2 (- n 1)) (car a)) (b2u (- n 1) (cdr a)))]))
(define (u2b n x)
  (define (ravi y)
    (cond[(= y 0) '(0)]
         [(= y 1) '(1)]
         [else ( append (ravi (quotient y 2)) (list (remainder y 2)))]))
  (cond[(< (- n (length (ravi x))) 0) #f]
       [(append (make-list (- n (length (ravi x))) 0) (ravi x))]))
       
      
;q2
(define (b2s n a )
  (define (helper k b)
 (cond[(= k 0) 0]
      [else ( + ( * (power 2 (- k 1)) (car b)) (helper (- k 1) (cdr b)))]))
  (- (helper (- n 1) (cdr a)) ( * (power 2 (- n 1)) (car a))))
(define (sum2 n )
  (cond[(= n 0) 1]
       [else (+ (power 2 n) (sum2 (- n 1)))]))
(define (ravi y)
    (cond[(= y 0) '(0)]
         [(= y 1) '(1)]
         [else ( append (ravi (quotient y 2)) (list (remainder y 2)))]))
 ( define (s2b1 n y) 

               (cond[(< y 0) (append (cons 1 (append (make-list (- (- n 1) (length (ravi (+ (power 2 (- n 1)) y)))) 0) (ravi (+ (power 2 (- n 1)) y)))))]
                    [else (u2b n y)]))


(define (s2b n x)
  ( define (s2b1 y) 

               (cond[(< y 0) (append (cons 1 (append (make-list (- (- n 1) (length (ravi (+ (power 2 (- n 1)) y)))) 0) (ravi (+ (power 2 (- n 1)) y)))))]
                    [else (u2b n y)]))
  (cond[(and (> x (-(power 2 (- n 1)))) (< x (sum2 (- n 1)))) (s2b1 x)]
       [else #f]))
(define (u-add n a b)
   (define (add a b)
    (cond[(null? (cdr a)) (list(+ (car b) (car a)))]
         [else (cons (+ (car a) (car b)) (add (cdr a) (cdr b)))]))
  (define (ravi l)
 (cond[ (null? (cdr l)) (append (list(modulo (car l) 2)))] 
      [(> (car l) 1)  (append (list( modulo  (car l)  2)) ( ravi (add (cdr l) (cons 1 (make-list (- (length (cdr l)) 1) 0)))))]
      [else (append (list(modulo (car l) 2)) (ravi (cdr l)))]))
  (reverse (ravi (reverse (add a b)))))
(define (u-sub n a b)
  (cond[(< (b2u n a) (b2u n b)) (make-list n 0)]
       [else  (u-add n a (s2b1 n (* -1 (b2u n b))))]))