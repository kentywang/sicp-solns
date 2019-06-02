#lang sicp

;;; Deps

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

;;; Main

(define (union-set s t)
  (if (null? s)
      t
      (union-set (cdr s) (adjoin-set (car s) t))))

;; time = O(s^2t), see edit.
;; space = O(s + t), or O(1) if discounting new list.

;;; Tests

(define x '(a b c d))
(define y '(d e f g))

(union-set x y)
(union-set x '())
(union-set x x)

;; Edit: Although my solution is simple, it's not the most efficient since it
;; iterates through the newly added elements from the first set repeatedly.

;; Better solution would be to not use adjoin-set and move the cons out of the
;; second argument of union-set.