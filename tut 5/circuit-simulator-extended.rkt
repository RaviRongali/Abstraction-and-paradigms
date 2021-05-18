
#lang racket

(require racket/mpair)

(define inverter%
  (class object%
    
    (init-field input)   ; input and output are
                         ; public fields
    (init-field output)
  
    (define inverter-delay 1) 
     
    (super-new)
    
    (define (logical-not s)
      (cond ((equal? s 0) 1)
            ((equal? s 1) 0)
            (else 'undefined)))
    
    ; invert-input is an action that is attached
    ; to the input wire of any inverter. It is invoked by
    ; a signal coming to the input wire.

    ; when invoked, it  installs another action
    ; on the event queue at current-time+inverter delay
    
    ; this is invoked by a time tick. when invoked, it
    ; sends a signal to the output wire
    
    (define (invert-input)
      (let ((new-value (logical-not
                        (send input get-signal))))
        (send the-queue add-to-queue-after inverter-delay
            (lambda ()
              (send output set-signal! new-value)))))
        
    (send input add-action! invert-input)
    'ok)) 

(define and-gate%
  (class object%
    (init-field a1)
    (init-field a2)
    (init-field output)
    (define and-gate-delay 1)
    (super-new)
    
    (define (and-action-procedure)
      (let ((new-value (logical-and (send a1 get-signal)
                                    (send a2 get-signal))))
        (send the-queue add-to-queue-after and-gate-delay
           (lambda ()
             (send output set-signal! new-value)))))
    
    (define (logical-and s1 s2)
      (cond ((and (equal? s1 0) (equal? s2 0))  0)
            ((and (equal? s1 0) (equal? s2 1))  0)
            ((and (equal? s1 1) (equal? s2 0))  0)
            ((and (equal? s1 1) (equal? s2 1))  1)
            (else `undefined)))
    
    (send a1 add-action! and-action-procedure)
    (send a2 add-action! and-action-procedure)
    'ok))



(define or-gate%
  (class object%
    (init-field a1)
    (init-field a2)
    (init-field output)
    (define or-gate-delay 1)
    (super-new)
    
    (define (or-action-procedure)
      (let ((new-value (logical-or (send a1 get-signal) (send a2 get-signal))))
        (send the-queue add-to-queue-after or-gate-delay
                     (lambda ()
                       (send output set-signal! new-value)))))
    
    (define (logical-or s1 s2)
      (cond ((and (equal? s1 0) (equal? s2 0))  0)
            ((and (equal? s1 0) (equal? s2 1))  1)
            ((and (equal? s1 1) (equal? s2 0))  1)
            ((and (equal? s1 1) (equal? s2 1))  1)
            (else `undefined)))
    
    (send a1 add-action! or-action-procedure)
    (send a2 add-action! or-action-procedure)
    'ok))

(define half-adder%
  (class object%
    (init-field a)
    (init-field b)
    (init-field s)
    (init-field c)
    (super-new)
    
    (define d (make-object  wire% "d"))
    (define e (make-object  wire% "e"))
    (make-object probe%  d the-queue)
    (make-object probe%  e the-queue)
    (make-object or-gate% a b d)
    (make-object and-gate% a b c)
    (make-object inverter% c e)
    (make-object and-gate% d e s)))
  
(define full-adder%
  (class object%
   
    (init-field a)
    (init-field b)
    (init-field ci)
    (init-field s)
    (init-field co)
    
    (define d (make-object  wire% "d"))
    (define e (make-object  wire% "e"))
    (define f (make-object  wire% "f"))
    
    (super-new)
    
    (make-object half-adder% b ci d e)
    (make-object half-adder% a d s f)
    (make-object or-gate%  f e co)))

(define wire%
  (class object%
    (init-field name)
    (init-field (signal-value 'undefined))
    (init-field (actions '()))

    (super-new)

    (define/public (get-signal) signal-value)
    (define/public (get-name) name) 

    (define/public (set-signal! new-value)
      (cond [(not (equal? new-value signal-value))
             (begin
               (set! signal-value new-value)
               (call-each actions))]))

    (define (call-each actions)
      (cond [(not (null? actions))
             (begin
               ((car actions))
               (call-each (cdr actions)))]))

    
    (define/public (add-action! action)
      (set! actions (cons action actions)))))
    


(define event-queue%
  (class object% 
    (init-field (current-time 0))
    (init-field (farthest-event-time 0))
    (init-field (queue (make-vector 200 '())))
    
    (super-new)   
    
    (define/public (add-to-queue! time action)
      (begin
        (vector-set! queue time
                     (mappend! (vector-ref queue time)
                               (mlist action)))
        (cond ((> time farthest-event-time)
               (set! farthest-event-time time)))))
      
    (define (queue-empty?)
      (> current-time farthest-event-time))
    
    (define (increment-time!)
      (set! current-time (+ current-time 1)))
    
    (define (next-item-in-queue)
      (vector-ref queue current-time))
    
    (define/public (get-time)
      current-time)

    (define/public (propagate)
      (if (queue-empty?)
          'done
          (let ((action-list (next-item-in-queue)))
            (execute! action-list)
            (increment-time!)
            (propagate))))

    (define (execute! action-list)
      (cond [(not (null? action-list)) 
             (begin
               ((mcar action-list)) 
               (execute! (mcdr action-list)))]))
    
    (define/public (add-to-queue-after delay action)
      (add-to-queue!
            (+ delay (get-time)) action))))
    

    
(define probe% 
  (class object%
    (init-field wire)
    (init-field queue)

    (super-new)
    
    (send wire add-action! 
            (lambda ()
              (newline)
              (display "time = ")
              (display (send queue get-time ))
              (newline)
              (display "wire-name = ")
              (display (send wire get-name))
              (newline)
              (display "New-value = ")
              (display (send wire get-signal))
              (newline)
              (newline)))))
  
    



(define the-queue (make-object event-queue%))

(define a (make-object  wire% "a"))
(define b (make-object  wire% "b"))
(define s (make-object  wire% "s"))
(define c (make-object  wire% "c"))

(make-object probe%  a the-queue)
(make-object probe%  b the-queue)
(make-object probe%  s the-queue)
(make-object probe%  c the-queue)

(make-object half-adder% a b s c)

(send the-queue add-to-queue! 0 (lambda () (send b set-signal! 1)))
(send the-queue add-to-queue! 0 (lambda () (send a set-signal! 1)))


(send the-queue propagate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  




