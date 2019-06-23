#lang sicp

(define (count-pairs x)
  (let ((history '()))
    (define (add-to-list p)
      (set! history (cons p history)))
    (define (recur p)
      (if (or (not (pair? p))
              (memq p history))
          0
          (begin (add-to-list p)
                 (+ (recur (car p))
                    (recur (cdr p))
                    1))))
    (recur x)))

;; Time = O(n^2) because need to run through list of n pairs for each step.
;; Space = O(n) for recursive stacks and keeping history data structure.

;;; Tests

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (list x y))
(define u (list x x))

(define v (cons 'e 'f))
(define w (cons v v))
(define r (cons w w))

(define s (cons 'g '()))
(define t (cons 'h '()))
(set-cdr! s t)
(set-cdr! t s)

(= 2 (count-pairs x))
(= 6 (count-pairs z))
(= 4 (count-pairs u))

(= 3 (count-pairs r))
(= 2 (count-pairs s))
