#lang racket
;;q14
(define (my_exp n l)
  (define (helper l)
  (if (null? l ) 1
      (* (expt n (car l)) (sqr (helper (cdr l))))))
  (helper (reverse l)))
