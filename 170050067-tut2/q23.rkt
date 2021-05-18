#lang racket
;;23
(define (noofzeros l)
  (if (null? l) 0
      (if (= (car l) 0) (+ 1 (noofzeros (cdr l)))
          (noofzeros (cdr l)))))  
(define (noofones l)
  (if (null? l) 0
      (if (= (car l) 1) (+ 1 (noofones (cdr l)))
          (noofones (cdr l)))))

(define (requires x1 x2 l1 l2)
  (if (null? l1) l2
  (if (and (= (noofzeros (car l1)) x1) (= (noofones (car l1)) x2)) (requires x1 x2 (cdr l1) (append (list (car l1)) l2))
      (requires x1 x2 (cdr l1) l2))))
(define (shuffle l1 l2)
  (define (helper l1 l2 l ans)
    (if (null? l) ans
        (if (= (car l) 0) (helper (cdr l1) l2 (cdr l) (append ans (list (car l1))))
            (helper l1 (cdr l2) (cdr l) (append ans (list (car l2)))))))
  (define (helper1 n y)
    (if (null? n) y
        (helper1 (cdr n) (append (list (helper l1 l2 (car n) '())) y))))
  
  (let [(n (requires (length l1) (length l2) (gc (+ (length l1) (length l2))) '()))]
    (helper1 n '())))
