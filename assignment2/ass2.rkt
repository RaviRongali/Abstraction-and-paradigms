#lang racket
(require "declarations.rkt")
(require "utilities.rkt")


 (define x (maketree "(a|b)*bba|cc*"))
(define (Nullable t)
  (define (helper t)
    (cond[(Epsilon? t) (list (list (Epsilon-n t) #t))]
         [(Literal? t) (list (list (Literal-n t) #f))]
         [(Or? t) (append  (list (list (Or-n t) #f)) (helper (Or-t1 t)) (helper(Or-t2 t)))]
         [(Then? t) (append (list  (list (Then-n t) #f)) (helper (Then-t1 t)) (helper(Then-t2 t)))]
         [(Star? t) (append (list (list (Star-n t) #t)) (helper (Star-t t)))]))
  (helper t))
(define (nullable? t)
  (if (or (Star? t) (Epsilon? t)) #t
      #f))
(define (Firstpos t)
  (define (helper t)
    (cond[(Epsilon? t) null]
         [(Literal? t) (list (Literal-n t))]
         [(Then? t) (if (nullable? (Then-t1 t)) (append (helper (Then-t1 t)) (helper (Then-t2 t)))
                        (helper (Then-t1 t)))]
         [(Or? t) (append (helper (Or-t1 t)) (helper (Or-t2 t)))]
         [(Star? t) (helper (Star-t t))]))
  (cond[(Epsilon? t) (list (list (Epsilon-n t)))]
       [(Literal? t) (list (append (list (Literal-n t)) (helper t)))]
       [(Then? t) (append (list (append (list (Then-n t))(helper t))) (Firstpos (Then-t1 t)) (Firstpos (Then-t2 t)))]
       [(Or? t) (append (list (append (list (Or-n t)) (helper t))) (Firstpos (Or-t1 t)) (Firstpos (Or-t2 t)))]
       [(Star? t) (append (list (append (list (Star-n t)) (helper t))) (Firstpos (Star-t t)))]))
(define (Lastpos t)
  (define (helper t)
    (cond[(Epsilon? t) null]
         [(Literal? t) (list (Literal-n t))]
         [(Then? t) (if (nullable? (Then-t2 t)) (append (helper (Then-t1 t)) (helper (Then-t2 t)))
                        (helper (Then-t2 t)))]
         [(Or? t) (append (helper (Or-t1 t)) (helper (Or-t2 t)))]
         [(Star? t) (helper (Star-t t))]))
  (cond[(Epsilon? t) (list (list (Epsilon-n t)))]
       [(Literal? t) (list (append (list (Literal-n t)) (helper t)))]
       [(Then? t) (append (list (append (list (Then-n t))(helper t))) (Lastpos (Then-t1 t)) (Lastpos (Then-t2 t)))]
       [(Or? t) (append (list (append (list (Or-n t)) (helper t))) (Lastpos (Or-t1 t)) (Lastpos (Or-t2 t)))]
       [(Star? t) (append (list (append (list (Star-n t)) (helper t))) (Lastpos (Star-t t)))]))
(define (Followpos t)
  (define (helper1 t)
    (cond[(Epsilon? t) null]
         [(Literal? t) (list (Literal-n t))]
         [(Then? t) (if (nullable? (Then-t1 t)) (append (helper1 (Then-t1 t)) (helper1 (Then-t2 t)))
                        (helper1 (Then-t1 t)))]
         [(Or? t) (append (helper1 (Or-t1 t)) (helper1 (Or-t2 t)))]
         [(Star? t) (helper1 (Star-t t))]))
  (define (helper2 t)
    (cond[(Epsilon? t) null]
         [(Literal? t) (list (Literal-n t))]
         [(Then? t) (if (nullable? (Then-t2 t)) (append (helper2 (Then-t1 t)) (helper2 (Then-t2 t)))
                        (helper2 (Then-t2 t)))]
         [(Or? t) (append (helper2 (Or-t1 t)) (helper2 (Or-t2 t)))]
         [(Star? t) (helper2 (Star-t t))]))
  (define (append1 x y)
    (map (lambda (x) (append x y)) x))
  (cond[(Then? t) (append (append1 (helper2 (Then-t1 t)) (helper1 (Then-t2 t))) (Followpos (Then-t1 t)) (Followpos (Then-t2 t)))]
       [(