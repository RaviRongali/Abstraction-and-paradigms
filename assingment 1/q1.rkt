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
  (if (= n (length (ravi x))) (ravi x)
      #f))