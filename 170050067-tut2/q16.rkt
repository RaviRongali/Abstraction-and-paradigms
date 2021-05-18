#lang racket
;;q16
(define (taile-reverse l)
  (define (helper l1 l2)
    (if (null? l1) l2
        (helper (cdr l1) (append (list (car l1)) l2))))
  (helper l '()))
