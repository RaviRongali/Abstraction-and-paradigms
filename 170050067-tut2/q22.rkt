#lang racket
;;q22
(define (lcs l1 l2)
  (define (helper l1 l2 l3)
    (if (or (null? l2) (null? l1)) l3
        (if (= (car l1) (car l2)) (helper (cdr l1) (cdr l2) (append l3 (list (car l1))))
            (if (< (length (helper (cdr l1) l2 '())) (length (helper l1 (cdr l2) '()))) (helper l1 (cdr l2) l3)
                (helper (cdr l1) l2 l3)))))
  (helper l1 l2 '()))
