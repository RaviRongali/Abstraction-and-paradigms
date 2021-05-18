#lang racket
;;q1,q2,q3
(define (nth k l)
  (define (helper k l)
    (if (= k 1) (car l)
        (helper (- k 1) (cdr l))))
  (helper k l))
