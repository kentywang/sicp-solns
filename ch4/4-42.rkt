#lang sicp


;;; Deps

(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (xor a b) ; or (not (eq? a b))
  (or (and a (not b))
      (and (not a) b)))

;;; 4.42

;; amb makes this easy!

(define (results)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require
     (distinct? (list betty ethel joan kitty mary)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

; ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))

;;; 4.43

(define (dads)  
  (let ((d-moore (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
        (d-downing (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
        (d-hall (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
        (d-barnacle (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
        (d-parker (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
        (y-moore (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
        (y-downing (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
        (y-hall (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
        (y-barnacle (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa))
        (y-parker (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa)))
    (require (and (eq? d-moore 'mary) ; girls
                  (not (eq? d-downing 'mary))
                  (not (eq? d-hall 'mary))
                  (not (eq? d-barnacle 'mary))
                  (not (eq? d-barnacle 'mary))
                  (not (eq? y-moore 'mary)))) ; yatchs 
    (require (and (eq? y-barnacle 'gabrielle) ; yatchs
                  (not (eq? y-moore 'gabrielle))
                  (not (eq? y-downing 'gabrielle))
                  (not (eq? y-hall 'gabrielle))
                  (not (eq? y-parker 'gabrielle))
                  (not (eq? d-barnacle 'gabrielle)))) ; girls
    (require (and (eq? y-moore 'lorna) ; yatchs
                  (not (eq? y-barnacle 'lorna))
                  (not (eq? y-downing 'lorna))
                  (not (eq? y-hall 'lorna))
                  (not (eq? y-parker 'lorna))
                  (not (eq? d-moore 'lorna)))) ; girls
    (require (and (eq? y-hall 'rosalind) ; yatchs
                  (not (eq? y-barnacle 'rosalind))
                  (not (eq? y-downing 'rosalind))
                  (not (eq? y-moore 'rosalind))
                  (not (eq? y-parker 'rosalind))
                  (not (eq? d-hall 'rosalind)))) ; girls
    (require (and (eq? y-downing 'melissa) ; yatchs
                  (not (eq? y-barnacle 'melissa))
                  (not (eq? y-hall 'melissa))
                  (not (eq? y-moore 'melissa))
                  (not (eq? y-parker 'melissa))
                  (not (eq? d-downing 'melissa)))) ; girls
    (require (and (eq? d-barnacle 'melissa) ; girls
                  (not (eq? d-moore 'melissa))
                  (not (eq? d-downing 'melissa))
                  (not (eq? d-hall 'melissa))
                  (not (eq? d-parker 'melissa))
                  (not (eq? y-barnacle 'melissa)))) ; yachts
    (require (and (not (eq? d-parker 'gabrielle))
                  (cond ((eq? d-moore 'gabrielle)
                         (eq? d-moore y-parker))
                        ((eq? d-hall 'gabrielle)
                         (eq? d-hall y-parker))
                        ((eq? d-barnacle 'gabrielle)
                         (eq? d-barnacle y-parker))
                        ((eq? d-downing 'gabrielle)
                         (eq? d-downing y-parker)))))
;   (require
;      (and (distinct? (list d-moore d-downing d-hall d-barnacle d-parker))
;           (distinct? (list y-moore y-downing y-hall y-barnacle y-parker))))
    
    (list (list 'moore d-moore)
          (list 'downing d-downing)
          (list 'hall d-hall)
          (list 'barnacle d-barnacle)
          (list 'parker d-parker))))