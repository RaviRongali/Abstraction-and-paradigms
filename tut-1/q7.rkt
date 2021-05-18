#lang racket

(define (gcd a b)
  (cond[(= a 0) b]
       [(= b 0) a]
       [(< a b) (gcd b a)]
       [(and (>= a 0) (>= b 0)) (gcd (- a b) b)]))

(define (simple p)
  (cond[(let ([gcdab (gcd (car p) (cdr p))])
          (cons (quotient (car p) gcdab) (quotient (cdr p) gcdab)))]))

(define (addd p1 p2)
  (cond[(let ([p (+ (* (car p1) (cdr p2)) (* (car p2) (cdr p2)))]
              [q (* (cdr p1) (cdr p2))])
          (simple (cons p q)))]))

(define (multd p1 p2)
  (cond[(let ([p (* (car p1) (car p2))]
              [q (* (cdr p1) (cdr p2))])
          (simple (cons p q)))]))

(define (divd p1 p2)
  (cond[(let ([p (* (car p1) (cdr p2))]
              [q (* (cdr p1) (car p2))])
          (simple (cons p q)))]))
