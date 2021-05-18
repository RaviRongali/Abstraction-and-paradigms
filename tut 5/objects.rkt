#lang racket

; (define size 100) defines a private field. This initializes the size
; field of all  objects to 100. Now the only way  of changing the size
; field is through a method.
 
; A  field  defined  through  a  define  can  be  given  a  per-object
; initialization through a init.

;     (init x)
;     (define size x) 

; A field  introduced a  define is  private in the  sense it  can't be
; accessed with a get-field or set with a set-field!

; (field [size 100]) defines a public field. This initializes the size
; field  of  all  objects  to  100.  It  can  be  given  a  per-object
; initialization through a init.

;     (init x)
;     (field [size x]) 

; A field introduced through a  field can be accessed with a get-field
; and set through a set-field!

; An init-field  can be given a  default initialization as  well as be
; given a per-object initialization during object creation. It defines
; a public field.

;;;;;;;;;Creation of a protected account without inheritence ;;;;;;;;;

;(define account% 
;  (class object% 
;    (init passwd)            ; passwd and bal will be initialised
                              ; during object creation
;    (define password passwd) 
;    (init ach)
;    (init-field [account-holder ach])   ; This necessarily has to be a per-object initialization
;    (init bal)
;    (init-field [balance 0])
;    (init-field (balance 10000)) ; balance is public and password is private. 
                                 ; If instead of bal we had a constant, then 
                                 ; this would have meant a class initialization
;    (super-new)
;    (define/public (show) balance)
;    (define/public (withdraw amount)
;      (if (<= amount balance)
;          (begin
;            (set! balance (- balance amount))
;            balance)
;          "Insufficient funds"))
;    (define/public (deposit amount)
;      (begin
;        (set! balance (+ balance amount))
;        balance))
;    (display (show))))

;; (define my-account 
;;   (new account%
;;        [balance 0]
;;        [passwd "%&%^%&***"]
;;        [ach 'Vinayak])) ; initialize passwd, override the default value
		           ; of balance

;(get-field account-holder my-account)  
;(set-field! account-holder my-account `AS)  
;(send my-account show)
;(send my-account withdraw 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define account% 
  (class object%                     ;Inherits from object%, 
    (init-field account-holder)      ;public data member
    (init-field balance)             ;public data member. Just for
                                     ;illustration. Making balance public
                                     ;is a wrong thing to do.
    (super-new)
    (displayln this)
    (define/public (show) balance)       ;public function 
    (define/public (withdraw amount)     ;public function
      (if (<= amount balance)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))
    (define/public (deposit amount)      ;public function
      (begin
        (set! balance (+ balance amount))
        balance))))

(define my-account                       ;object creation 
  (new account% 
       [account-holder 'AS] 
       [balance  50000]))
(send my-account show);initialize  account-holder and balance


(define protected-account%
  (class account%                      ;inherited from account%
    (init passwd)                      ;init for initialization
    (init acholder)
    (init bal)
    (define password passwd)           ;private data member initialized with passwd
    (super-new [account-holder acholder] [balance bal]) ; superclass initialization
    (displayln this)          ; this refers to the object being initialized
    (define/override (withdraw amount passd)
      (cond[(string=? passd password) (super withdraw amount)]
           [else "Wrong Password"]))))

; Either of the two below work for object creation.

;(define pa (make-object protected-account% "hfksdah" 'AS 10000))

(define pa (new protected-account% [passwd "hfksdah"]
                [acholder 'AS] [bal 10000]))

; To set or get a public member. Once again, this is just for
; illustration. It is wrong to make balance a public member.

(set-field! balance pa 500)
(get-field balance pa)
; To invoke a member function
(send pa show)

(send pa withdraw 10 "hfksdah")

(define test%
  (class object%
    (super-new)
    (init x)
    (define kk x)
    (define/public (size) kk)
    (displayln this)))
(define ob (new test%
                [x 5]))
(send ob size)