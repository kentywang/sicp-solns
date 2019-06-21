#lang sicp

;; Dep
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;; Main
(define (adjoin-set x s)
  (cond ((null? s) '(x)) ; Not in set.
        ((< x (car s)) (cons x s)) ; Bigger than anything in set.
        ((= x (car s)) s)
        (else (cons (car s)
                    (adjoin-set x (cdr s))))))

;; Worst case, if element is not in set and larger than anything in set:
;; time = O(n)
;; space = O(n)

;; Typical case is probably O(n/2).

;;; Tests

(define a '(1 4 9 16 25 36 49))

;; What if an element is already in the set?
(adjoin-set 49 a)

;; What if the set is empty?
(adjoin-set 2 '())

;; What if the element's value lies in the middle of the set?
(adjoin-set 10 a)