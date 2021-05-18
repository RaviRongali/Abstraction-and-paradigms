#lang racket
;;q6
(define (remove-consecutive l)
  (if (null? l) '()
  (if (and (not (null? (cdr l))) (eq? (car l) (cadr l))) (remove-consecutive (cdr l))
      (append (list (car l)) (remove-consecutive (cdr l))))))

