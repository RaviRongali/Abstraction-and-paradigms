#lang racket

(define (fib n)
  (fib-tail n 1 1))
(define (fib-tail n c1 c2)
  (if (= n 1) c2
      (fib-tail (- n 1) c2 (+ c1 c2))))
(define (fibo n)
  (if (or (= n 0) (= n 1)) 1
      (+ (fibo(- n 1)) (fibo(- n 2)))))

(define (fib-lightning n)
  (define (helper p q n)
    (if (= n 1) (cons p q)
        (if (odd? n) (cons (cons (+ (* (car (car (helper p q (- n 1)))) (car p)) (* (cdr (car (helper p q (- n 1)))) (car q)))
                                 (+ (* (car (car (helper p q (- n 1)))) (cdr p)) (* (cdr (car (helper p q (- n 1)))) (cdr q))))
                           (cons (+ (* (car (cdr (helper p q (- n 1)))) (car p)) (* (cdr (cdr (helper p q (- n 1)))) (car q)))
                                 (+ (* (car (cdr (helper p q (- n 1)))) (cdr p)) (* (cdr (cdr (helper p q (- n 1)))) (cdr q)))))
        
        (helper (cons (+ (* (car p) (car p)) (* (cdr p) (car q))) (+ (* (car p) (cdr p)) (* (cdr p) (cdr q))))
                (cons (+ (* (car q) (car p)) (* (car q) (cdr q))) (+ (* (car q) (car p)) (* (cdr q) (cdr q))))
                (quotient n 2)))))
  (if (or (= n 0) (= n 1)) 1
  (car (car (helper (cons 1 1) (cons 1 0) n)))))

