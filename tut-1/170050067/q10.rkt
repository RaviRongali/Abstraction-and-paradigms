#lang racket
(define (cansurvive pos n)
(define (ans n k)
  (if (= n 1) 0
       (modulo (+ (ans (- n 1) k) 2) n)))
  (if (= (+ (ans n 3) 1) pos) #t
      #f))
