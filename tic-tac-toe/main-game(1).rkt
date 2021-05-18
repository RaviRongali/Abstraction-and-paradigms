#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require rsound)
;;;;;;
(struct cell (state image) #:transparent);a cell has a state and image of the state
(define board-length 10)
;;;;;;;;;;;;;;;
(define (make-2d-vector r c initial);function to create a 2d-vector
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val))))
;;;;;;;;;;;;;;;;;;
(define initial (cell 3 (place-image (frame (square 40 "outline" "black")) 20 20 (square 40 "solid" "blue"))));initial value of a cell
;;;;;;;
(define init-board;defining a board
  (make-2d-vector 10 10 initial))
;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;

(define (stack imgs)
  (cond [(empty? (rest imgs)) (first imgs)]
        [else (overlay/xy (first imgs) 0 40 (stack (rest imgs)))]))

(define (list_vectors 2d-vec x y)
  (define ans null)
  (define (helper p)
    (cond [(equal? p y) (reverse ans)]
          [else (begin (set! ans (append (list (cell-image (2d-vector-ref 2d-vec x p))) ans)) (helper (+ p 1)))]))
  (helper 0))

(define world
  (beside/align "bottom"
                (stack (list_vectors init-board 0 10))
                (stack (list_vectors init-board 1 10))
                (stack (list_vectors init-board 2 10))
                (stack (list_vectors init-board 3 10))
                (stack (list_vectors init-board 4 10))
                (stack (list_vectors init-board 5 10))
                (stack (list_vectors init-board 6 10))
                (stack (list_vectors init-board 7 10))
                (stack (list_vectors init-board 8 10))
                (stack (list_vectors init-board 9 10))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct player (turn score) #:transparent);a player has a turn and a score
(define player1 (player #t 0))
(define player2 (player #f 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render world);draws the current world
  world)
;;;;;;;;;;;;;;;;;;;;;
(define (make_horizontal rows)
  (define (horizontal_vector c )
    (define ans null)
    (define (helper x)
      (cond[(equal? x 10) ans]
           [else (begin (set! ans (append (list (cell-state (2d-vector-ref init-board x c))) ans)) (helper (+ x 1)))]))
    (helper 0))
  (define ans null)
  (define (helper x)
    (cond[(equal? x (+ rows 1)) ans]
         [else (begin (set! ans (append (list (horizontal_vector x)) ans))
                      (helper (+ x 1)))]))
  (helper 0))
;;;;;;;;;
(define (make_vertical coloums)
  (define (vertical_vector c)
    (define ans null)
    (define (helper y)
      (cond[(equal? y 10) ans]
           [else (begin (set! ans (append (list (cell-state (2d-vector-ref init-board c y))) ans)) (helper (+ y 1)))]))
    (helper 0))
  (define ans null)
  (define (helper x)
    (cond[(equal? x (+ coloums 1)) ans ]
         [else (begin (set! ans (append (list (vertical_vector x)) ans))
                      (helper (+ x 1)))]))
  (helper 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;left to right-up
(define (lefttoright l)
  (define (helper y)
  (define ans null)
  (define (helper1 x y)
  (cond[(equal? y -1) ans]
       [else (begin (set! ans (append (list (cell-state (2d-vector-ref init-board x y))) ans))
                    (helper1 (+ x 1) (- y 1)))]))
  (helper1 0 y))
  (define ans null)
  (define (helper2 x)
    (cond[(equal? x l) ans]
         [else (begin (set! ans (append (list (helper x)) ans))
                      (helper2 (+ x 1)))]))
  (helper2 0))


;;;;;;;;;;;;right to left-down
(define (righttodown l)
  (define (helper1 y)
  (define ans null)
  (define (helper3 x y)
  (cond[(equal? y 10) ans]
       [else (begin (set! ans (append (list (cell-state (2d-vector-ref init-board x y))) ans))
                    (helper3 (- x 1) (+ y 1)))]))
  (helper3 9 y))
  (define ans null)
  (define (helper4 x)
    (cond[(equal? x (+ l 1)) ans ]
         [else (begin (set! ans (append (list (helper1 x)) ans))
                      (helper4 (+ x 1)))]))
  (helper4 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (lefttobottomup l);;;;right to up
  (define (helper x)
  (define ans null)
  (define (helper1 x y)
    (cond[(equal? x 10) ans]
         [else (begin (set! ans (append (list (cell-state (2d-vector-ref init-board x y))) ans ))
                             (helper1 (+ x 1) (+ y 1)))]))
  (helper1 x 0))
  (define ans null)
  (define (helper1 x)
  (cond[(equal? x (+ l 1)) ans]
       [else (begin (set! ans (append (list (helper x)) ans))
                    (helper1 (+ x 1)))]))
  (helper1 0))

;;;;;;;;;;;;;;left to down
(define (lefttobottomdown l)
  (define (helper y)
  (define ans null)
  (define (helper1 x y)
    (cond[(equal? y 10) ans]
         [else (begin (set! ans (append (list (cell-state (2d-vector-ref init-board x y))) ans ))
                             (helper1 (+ x 1) (+ y 1)))]))
  (helper1 0 y))
  (define ans null)
  (define (helper2 x)
    (cond[(equal? x (+ l 1)) ans]
         [else (begin (set! ans (append (list (helper x)) ans))
                      (helper2 (+ x 1)))]))
  (helper2 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (alllists l);combines all list
  (append* (list (lefttobottomup l) (lefttobottomdown l)
                                   (righttodown l) (lefttoright l) (make_vertical l) (make_horizontal l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (finder l);finds 5 consecutive elements
  (define x 1)
  (define (helper l)
    (cond[(null? (cdr l)) #f]
         [else (begin (if (and (equal? (car (cdr l)) 1) (equal? (car l) 1)) (set! x (+ x 1))
                     (set! x 1))
                 (if (eq? x 5) #t (helper (cdr l))))]))
  (helper l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define (endgame l);boolean answer for 5 in a row
  (define (helper l)
    (cond[(null? l) #f]
         [else (if (eq? (finder (car l)) #t) #t
                   (helper (cdr l)))]))
  (helper (alllists 9))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (my-mouse world x y me);handles and updates the current world
  (local ([define x0 (quotient x 40)]
          [define y0 (quotient y 40)])
    (cond [(mouse=? me "button-down")
           (cond [(and (player-turn player1) (equal? 3 (cell-state (2d-vector-ref init-board x0 y0))))
                  (begin (set! player2 (player #t (player-score player2)))
                         (set! player1 (player #f (player-score player1)))
                         (2d-vector-set! init-board x0 y0 (cell 0 (place-image
                                                                   (circle 15 "outline" "red")
                                                                   20
                                                                   20
                                                                   (place-image
                                                                    (square 40 "outline" "black")
                                                                    20
                                                                    20
                                                                    (square 40 "solid" "blue")))))
                         (if (helper (alllists 9)) (display "player1won")
                             (place-image (circle 15 "outline" "red")
                                          (+ (* x0 40) 20)
                                          (+ (* y0 40) 20)
                                          world)))]
                 [(and (player-turn player2) (equal? 3 (cell-state (2d-vector-ref init-board x0 y0))))
                  (begin (set! player1 (player #t (player-score player1)))
                         (set! player2 (player #f (player-score player2)))
                         (2d-vector-set! init-board x0 y0 (cell 1 (place-image
                                                                   (regular-polygon 30 3 "outline" "red")
                                                                   20
                                                                   20
                                                                   (place-image
                                                                    (square 40 "outline" "black")
                                                                    20
                                                                    20
                                                                    (square 40 "solid" "blue")))))
                         (place-image (regular-polygon 30 3 "outline" "red")
                                      (+ (* x0 40) 20)
                                      (+ (* y0 40) 20)
                                      world))]
                 [(and (player-turn player1) (not (equal? 3 (cell-state (2d-vector-ref init-board x0 y0)))))
                  (begin
                    (write "you cant place a circle there")
                    newline
                    world)]
                 [(and (player-turn player2) (not (equal? 3 (cell-state (2d-vector-ref init-board x0 y0)))))
                  (begin
                    (write "you cant place a triangle there")
                    newline
                    world)])]
          [else world])))

(big-bang world
  (to-draw render)
  (on-mouse my-mouse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (check-board 2d-vec);boolean answer returns true if 5elements in a row or column or diagonal is found consecutive
; (define (check-vertical 1d-vec))
; (define (check-horizontal 1d-vec))
; (define (check-diagonal element)))

;;;;;;;;;;;;;;;;;;;;;;;;(main functions)
