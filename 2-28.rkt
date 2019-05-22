#lang sicp

(define (fringe tree)
  (cond ((null? tree)
         nil)
        ((not (pair? tree))
         (list tree))
        (else
         (append (fringe (car tree))
                 (fringe (cdr tree))))))

;; time = O(n^2), since append [O(n)] is possibly called on every item.
;; space  = O(n), worst case if only one branch per level.

;;; Tests

(fringe (list (list 1 (list 2 3)) 4))

(define x
  (list (list 1 2) (list 3 4)))

(fringe x)

(fringe (list x x))

;; Edit: There's actually a way to do in constant time without append,
;; using nested recursion to reverse traverse and cons to build parts
;; of the list piecewise (list would not work, since it would nest):
(define (fringe L)
  (define (fringe-help L result)
    (if (null? L) ; if at end of the branch
        result
        (if (list? L) ; if the element is a list, not a number
            (fringe-help (car L) ; left branch
                         (fringe-help (cdr L) result)) ; right branch
            (cons L result)))) ; otherwise gather numbers into a list
  (fringe-help L '()))