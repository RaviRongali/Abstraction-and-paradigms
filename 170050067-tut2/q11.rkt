#lang racket
;;q11

(define (rotate l n)
  (define (helper1 l n)
    (if (= n 0) l
        (helper1 (append (cdr l) (list (car l))) (- n 1))))
  (define (helper2 l n)
    (if (= n 0) l
        (helper2 (append (list (car (reverse l))) (reverse (cdr (reverse l)))) (+ n 1))))
  
  (if (> n 0) (helper1 l n)
      (helper2 l n )))
