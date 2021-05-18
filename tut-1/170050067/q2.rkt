#lang racket
(define (gcd a b)
  (cond[(= a 0) b]
       [(= b 0) a]
       [(< a b) (gcd b a)]
       [(and (>= a 0) (>= b 0)) (gcd (- a b) b)]))
(define (has-a-solution a b c)
  (if (= (remainder c (gcd a b)) 0) #t #f))
