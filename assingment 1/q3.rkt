#lang racket
(define (n2m n m a)
    (cond[(= n m ) a]
         [(= (car a) 0) (append '(0) (n2m n (- m 1) a))]
         [(= (car a) 1) (append '(1) (n2m n (- m 1) a))]))