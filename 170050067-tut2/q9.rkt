#lang racket
;;q9

(define (drop l x)
  (define (helper l k x)
    (if (= x 1) (append k (cdr l))
        (helper (cdr l) (append k (list (car l))) (- x 1))))
  (helper l '() x))
