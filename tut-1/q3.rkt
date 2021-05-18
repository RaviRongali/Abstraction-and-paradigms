#lang racket
(define (div x y)
  (if (< x y) (cons 0 x)
      (let*([pair (div (quotient x 2) y)]
            [r (cdr pair)]
            [2q (* 2 (car pair))]
            [2r1 (if (even? x) (* 2 r) (+ (* 2 r) 1))])
            (if (>= 2r1 y) (cons (+ 2q 1) (- 2r1 y))
                (cons 2q 2r1)))))
