#lang racket

(define (how-many-ways n)
  (define (my-win n)
    (cond [(< n 3) 1]
          [(= n 3) 0]
          [(= n 4) 3]
          [else (+ (i-lose (- n 1))
                   (i-lose (- n 2))
                   (i-lose (- n 4)))]))
  (define (i-loose n)
    (cond [(< n 3) 0]
          [(= n 3) 2]
          [(= n 4) 0]
          [else (let ((f1 (my-win (- n 1)))
                      (f2 (my-win (- n 2)))
                      (f4 (my-win (- n 4))))
                  (if (and (> fw1 0) (> fw2 0) (> fw4 0))
                      (+ f1 f2 f4) 0))]))                     
  (my-win n))
