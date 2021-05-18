#lang racket
(define (remove-duplicates l)
  (define (helper l k)
    (if (null? l) k
        (if (not (member (car l) k)) (helper (cdr l) (append  k (list (car l))))
            (helper (cdr l) k))))
  (helper l '()))

