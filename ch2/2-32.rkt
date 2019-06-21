#lang sicp

;; Main
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (each)
                            (cons (car s) each))
                          rest)))))

;; time = O(n^2), since append reconstructs elements into bigger lists.
;; space = O(n)

;; Test
(subsets (list 1 2 3))

;; Works because problem is broken down into the next n - 1 elements,
;; whose solution subsets are each combined with the current element to
;; form the end set of subsets.