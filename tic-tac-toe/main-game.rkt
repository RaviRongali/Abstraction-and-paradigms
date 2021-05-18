#lang racket
(require 2htdp/image)
(require 2htdp/universe)
;;;;;;
(struct cell (state) #:transparent)
(define board-length 10)
;;;;;;;;;;;;;;;
(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val))))
;;;;;;;;;;;;;;;;;;
(define initial (square 40 "outline" "green"))

(define init-board
  (make-2d-vector 10 10 initial))
;;;;;;;;;;;;;;;;;;;;;
(define initial (cell null));initialization of initial

(define (board v)

;;;;;;;;;;;;;;;

;(define (stack imgs)
;  (cond [(empty? (rest imgs)) (first imgs)]
;        [else (overlay/xy (first imgs) 0 40 (stack (rest imgs)))]))
;
;(define (list_vectors l r y)
;  (define ans null)
;  (define (helper p)
;    (cond [(equal? p y) (reverse ans)]
;          [else (begin (set! ans (append (list (2d-vector-ref l r p)) ans)) (helper (+ p 1)))]))
;  (helper 0))
;
;(define world
;  (beside/align "bottom"
;                (stack (list_vectors init-board 0 10))
;                (stack (list_vectors init-board 1 10))
;                (stack (list_vectors init-board 2 10))
;                (stack (list_vectors init-board 3 10))
;                (stack (list_vectors init-board 4 10))
;                (stack (list_vectors init-board 5 10))
;                (stack (list_vectors init-board 6 10))
;                (stack (list_vectors init-board 7 10))
;                (stack (list_vectors init-board 8 10))
;                (stack (list_vectors init-board 9 10))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define player1 #t)
(define player2 #f)

(define (render world)
  world)
(define (my-mouse world x y me)
  (cond [(mouse=? me "button-down") (cond [player1 (begin (set! player2 #t)
                                                          (set! player1 #f)
                                                          (underlay/align/offset "left"
                                                                                 "top"
                                                                                 world
                                                                                 (* (quotient x 40) 40)
                                                                                 (* (quotient y 40) 40)
                                                                                 (circle 20 "outline" "red")))]
                                          [player2 (begin (set! player1 #t)
                                                          (set! player2 #f)
                                                          (underlay/align/offset "left"
                                                                                 "top"
                                                                                 world
                                                                                 (* (quotient x 40) 40)
                                                                                 (* (quotient y 40) 40)
                                                                                 (regular-polygon 36 3 "outline" "red")))])]
        [else world]))

(big-bang world
  (to-draw render)
  (on-mouse my-mouse))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                