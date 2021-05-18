#lang racket
(define (minimum p)
  (define (helper n l)
    (cond[(and (not (null? l)) (> (car l) n)) (helper n (cdr l))]
         [(if (or (null? l) (< n 0)) n
              (if (= n 0) 0
                  (if (> (helper n (cdr l)) (+ 1 (helper (- n (car l)) l))) (+ 1 (helper (- n (car l)) l))
                      (helper n (cdr l)))))]))
  (cond[(let ([l '(1 2 3 5 10 20 25 50)])
          (helper p (reverse l)))]))
