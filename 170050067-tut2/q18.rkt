#lang racket
;;q18
;(define (summand n)
(define (map1 x l1 l2)
  (if (null? l1) l2
      (map1 x (cdr l1) (append (list (append (car l1) (list x))) l2 )))) 
  
(define (summand n)
  (define (helper k1 k2 k3 l)
    (if (= k1 k2) (append (list (list n)) l) 
        (helper (- k1 1) k2 (+ k3 1) (append (map1 k3 (summand (- n k3)) '()) l))))
  (if (= n 1) (list (list 1))
      (helper n 1 1 '())))
