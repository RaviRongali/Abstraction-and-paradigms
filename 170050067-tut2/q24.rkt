#lang racket
;;24
(define (subsets l)
  (define (sub l1 l2)
  (if (null? l1) '()
      (if (= 1 (car l1)) (append (list (car l2)) (sub (cdr l1) (cdr l2)))
          (sub (cdr l1) (cdr l2)))))
  (define (helper n l1 l2)
    (if (null? n) l2
        (helper (cdr n) l1 (append l2 (list (sub (car n) l1))))))
  (let [(n (gc (length l)))]
    (helper n l '())))
