#lanfg racket
(define (is-perfect n)
  (define (sum l c)
    (if (null? l) c
         (sum (cdr l) (+ c (car l))))) 
  (define (all-factor n c)
    (if (=  n c)  '()
        (if (= 0 (remainder n c)) (append (list c) (all-factor n (+ c 1)))
               (append (all-factor n (+ c 1))))))
  (if (= n (sum (all-factor n 1) 0)) #t #f)
  )
