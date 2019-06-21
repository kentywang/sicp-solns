#lang sicp

;;; Same as non-duplicate.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set s t)
  (cond ((or (null? s) (null? t))
         '())
        ((element-of-set? (car s) t)
         (cons (car s)
               (intersection-set (cdr s) t)))
        (else
         (intersection-set (cdr s) t))))

;;; Different

(define adjoin-set cons) ; O(1) time now, vs O(n) before.

(define union-set append) ; O(n) time now, vs O(mn) before.

;; This representation is more efficient for getting bigger sets via adjoin-set
;; and union-set, but less efficient with finding subsets or elements, which
;; might take more than O(mn) and O(n) time, respectively, depending on the
;; number of duplicates.

;; It makes sense to use this representation when one adds to sets frequently
;; but checks for elements within sets seldomly.