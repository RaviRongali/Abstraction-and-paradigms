#lang racket
;;q8
(define (pack l)
  (define (helper l k)
    (if (null? l) k
        (if (null? k) (helper (cdr l) (append k (list (list (car l)))))
            (if (equal? (car (car (reverse k))) (car l))
                (helper (cdr l) (reverse (append (list (append (car (reverse k)) (list (car l)))) (cdr (reverse k)) )))
                (helper (cdr l) (reverse (append (list (list (car l))) (reverse k) )))))))
  (helper l '()))
