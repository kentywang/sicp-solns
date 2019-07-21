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

;; Strategy: use fork for each daughter of each father and fork for each
;; yacht, so 10 forks in total (we need to keep track of yatchs and daughters
;; to take advantage of last condition). We still need distinct requirement for
;; all daughters and yachts (even though we're given Moore's and Barnacle's)
;; because they need to be distinct throughout the entire set of comparision,
;; not just for the subset.

;; If we didn't know Mary was Moore's daughter, we'd could only use
;; (require (not (eq? d-moore y-moore))) and we'd have 8 different solutions.

(define (dads)  
  (let ((d-moore (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa)))
    (require (eq? d-moore 'mary))
    (let ((y-moore (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa)))
      (require (eq? y-moore 'lorna))
      (let ((y-barnacle (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa)))
        (require (eq? y-barnacle 'gabrielle))
        (let ((d-barnacle (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa)))
          (require (eq? d-barnacle 'melissa))
          (let ((y-hall (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa)))
            (require (eq? y-hall 'rosalind))
            (let ((d-hall (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa)))
              (require (not (eq? y-hall d-hall)))
              (let ((y-downing (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa)))
                (require (eq? y-downing 'melissa))
                (let ((d-downing (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa)))
                  (require (not (eq? y-downing d-downing)))
                  (let ((d-parker (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa)))
                    (require (not (eq? d-parker 'gabrielle)))
                    (let ((y-parker (amb 'mary 'gabrielle 'lorna 'rosalind 'melissa)))
                      (require (not (eq? d-parker y-parker)))
                      (require (cond ((eq? d-hall 'gabrielle)
                                      (eq? y-hall d-parker))
                                     ((eq? d-downing 'gabrielle)
                                      (eq? y-downing d-parker))))
                      (require (distinct? (list d-moore d-downing d-hall d-barnacle d-parker)))
                      (require (distinct? (list y-moore y-downing y-hall y-barnacle y-parker)))
                      (list (list 'moore d-moore)
                            (list 'downing d-downing)
                            (list 'hall d-hall)
                            (list 'barnacle d-barnacle)
                            (list 'parker d-parker)))))))))))))

;; Simpler solutions:
;; http://community.schemewiki.org/?sicp-ex-4.43
;; https://eli.thegreenplace.net/2008/01/05/sicp-section-432