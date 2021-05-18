#lang racket
;;20
(define (wc l)
  (define (helper l1 n1 n2)
    (if (null? l1) (cons n1 n2)
        (if (char=? (car l1) #\space) (helper (cdr l1) (+ 1 n1) n2)
            (if (char=? (car l1) #\newline) (helper (cdr l1) n1 (+ 1 n2))
                        (helper (cdr l1) n1 n2)))))
  (helper (string->list l) 0 0)
  )
