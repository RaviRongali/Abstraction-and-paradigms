#lang racket
(require 2htdp/image)
(require 2htdp/planetcute)
(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val))))
;;;;;;;;;;;;;;;;
(struct cell (image) #:transparent)

(define initial
  (square 40 "outline" "slateblue"))


(define init-board
  (make-2d-vector 10 10 initial))
(2d-vector-set! init-board 4 0 (square 40 "solid" "slateblue"))
    (define (draw-board squares)
      (cond
        [(null? (cdr squares)) (car squares)]
        [else (overlay/xy (car squares)
                          0 40
                          (draw-board (cdr squares)))]))

(define (list_vectors l r Y)
  (define ans null)
  (define (helper  p)
    (cond[(equal? p Y) (reverse ans)]
         [(begin (set! ans (append (list (2d-vector-ref l r p)) ans))
                 (helper  (+ p 1)))]))
  (helper  0))
 
         
  
;(define board 0)
  (define background
     (beside/align
       "bottom"
       (draw-board (list_vectors init-board 0 10))
       (draw-board (list_vectors init-board 1 10))
       (draw-board (list_vectors init-board 2 10))
       (draw-board (list_vectors init-board 3 10))
       (draw-board (list_vectors init-board 4 10))
       (draw-board (list_vectors init-board 5 10))
       (draw-board (list_vectors init-board 6 10))
       (draw-board (list_vectors init-board 7 10))
       (draw-board (list_vectors init-board 8 10))
       (draw-board (list_vectors init-board 9 10))))

;(
; 
;(define (my-mouse board x y me)
;  (cond[(mouse=? me "button-down" ) (set! board 
;       

