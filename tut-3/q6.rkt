#lang racket
(struct Gnode (val lst) #:transparent)
(define T (Gnode 1 (list
                    (Gnode 2 '())
                    (Gnode 3 (list (Gnode 5 '()) (Gnode 6 '())))
                    (Gnode 4 (list (Gnode 7 '()))))))


(define (lists t)
 ; (define (helper l t)
    (if (null? (Gnode-lst t)) (list (Gnode-val t))
  (map (lambda(x) (append (list (Gnode-val t)) x)) (map (lambda(x) (lists x)) (Gnode-lst t)))))
    
(define (root-leaf t)
  (define (helper l1 l2)
    (if (null? l2) l1
  (if (= (length (car l2)) 1) (helper (append l1 (list (car l2))) (cdr l2))
      (helper (append l1 (map (lambda(x) (append (list (car (car l2))) x)) (cdr (car l2)))) (cdr l2)))))
  (helper '() (lists t)))

(define (sum-to-list t)
  (define (helper l)
    (define n (length l))
    (define (helper1 n l)
      (if (null? l) 0
      (+ (* (car l) (expt 10 n)) (helper1 (- n 1) (cdr l)))))
    (helper1 (- n 1) l))
  (map (lambda(x) (helper x)) (root-leaf t)))
      
    
                    