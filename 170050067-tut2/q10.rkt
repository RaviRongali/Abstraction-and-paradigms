#lang racket
;;q10
(define (slice l n1 n2)
  (define (helper l m n1 n2 k)
    (if (null? l) m
    (if (and (or (= k n2) (< k n2)) (or (= k n1) (> k n1))) (helper (cdr l) (append m (list (car l))) n1 n2 (+ k 1))
        (helper (cdr l) m n1 n2 (+ k 1)))))
  (helper l '() n1 n2 1))
