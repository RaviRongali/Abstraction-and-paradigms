#lang racket
;;q15
(define (fewest-moves l)
  (define (helper l1)
    (cond[(null? l1) 0]
         [(and (= (car l1) 1) (> (length l1) 1)) (min (+ 1 (helper (cdr l1))) (+ 1 (helper (cdr (cdr l1)))))]
         [(and (= (car l1) 0) (> (length l1) 3)) (min (+ 1 (helper (cdr l1))) (+ 1 (helper (cdr (cdr (cdr (cdr l1)))))))]
         [(and (= (car l1) 1) (< (length l1) 2)) (+ 1 (helper (cdr l1)))]
         [(and (= (car l1) 0) (< (length l1) 4)) (+ 1 (helper (cdr l1)))]))
  (helper l))
