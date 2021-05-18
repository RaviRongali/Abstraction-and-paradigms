#lang racket
(define (modexp x y n)
  (if (= y 0) 1
      (if (even? y) (modulo (* (modexp x (quotient y 2) n) (modexp x (quotient y 2) n)) n)
          (modulo (* (modexp x (quotient y 2) n) (modexp x (quotient y 2) n) x) n))))
 
