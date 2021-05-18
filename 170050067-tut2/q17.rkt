#lang racket
;;q17
(define (rle l)
  (define (helper n l)
    (if (null? n) l
        (helper (cdr n) (append (list (append (list (car (car n))) (list (length (car n))))) l))))
  (let [(n (pac-conse l))]
    (helper n '())))

