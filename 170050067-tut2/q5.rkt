#lang racket
(define (flatten l)
  (if (null? l) '()
      (if (list? (car l)) (append (flatten (car l)) (flatten (cdr l)))
          (append  (list (car l)) (flatten (cdr l))))))

