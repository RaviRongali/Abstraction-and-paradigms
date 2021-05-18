#lang racket

(define (make-account balance password)

  (define (withdraw amount)
    (if (>= balance amount)
   (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))

  (define (deposit amount)
      (begin (set! balance (+ balance amount))
         balance))

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (lambda (x) "Unknown request"
                   ))))

  (define (verification p)
 (cond[ (eq? p password ) dispatch]
      [else (lambda (x) (lambda (x) "wrong pass" ))]))
  
verification)


(define nagadev (make-account 5000 'lukka))


(((nagadev 'lukka) 'withdraw1) 100)
(((nagadev 'lukka1) 'withdraw) 100)
(((nagadev 'lukka1) 'withdraw1) 100)
(define (joint-account make-account password newpassword)
  (define (verification p)
  (cond[(eq? p newpassword) (make-account password)]
      [else (lambda (x) (lambda (x) "wrong pass" ))]))
  verification)
(define ravi (joint-account nagadev 'lukka 'teja))
(((ravi 'teja) 'withdraw )100)
(define (inf)
  (define x1 0)
  (define x2 0)
  (define x3 0)
  (define (inf-h)
    (begin (set! x1 x2)
           (set! x2 x3)
           (set! x3 (read))
           (displayln (+ x1 x2 x3))
           (inf-h) ))
    (inf-h))
(define z 0)
(define (f1 x)
  (define tmp z)
  (begin (set! z x) tmp))



(define (f2 i q)
(define p i)
(if (> i 3) (f2 (- i 2) p)
(begin (+ p q))))
(define (g) '())
(define (main) (f2 4 g))
(main)
(g)




(define (f x)
  (define (p f) (f (* x x)))
  (define (g y)
    (cond [(= x 2) (begin (set! x (+ x 2))
                          (h (- y 1)))]))
  (define (h x)
   (begin (p (lambda (w) (+ w x)))))
  (g x))
(f 2)



 
  

