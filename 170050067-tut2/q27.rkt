#lang racket
;;27

(define (pascal n)
  (define (helper n l)
    (if (< n 0) l
        (helper (- n 1) (append (list (pascal1 n)) l))))
(define (pascal1 n)
  (define (helper l1 l2)
    (if (= (length l1) 1) l2
         (helper (cdr l1) (append (list (+ (car l1) (car (cdr l1)))) l2))))
  (if (= n 0) '(1)
      (append (list 1) (helper (pascal1 (- n 1)) '()) (list 1)))) 
  (helper n '()))
