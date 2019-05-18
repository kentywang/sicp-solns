#lang sicp

;;; Useful helpers

(define (last items)
  (let ((rest (cdr items)))
    (if (null? rest)  ; (cdr items) is empty list, so (car items) is last element.
        (car items)
        (last rest))))

;; time = O(n)
;; space = O(1)

(define (all-but-last items)
  (let ((rest (cdr items)))
    (if (null? rest)
        nil  ; (car items) is last element, so return nil since we don't want it.
        (cons (car items)
              (all-but-last rest)))))

;; time = O(n)
;; space = O(n)

;;; Main procedure

;; Recursive process, unoptimized
(define (reverse items)
  (let ((rest (cdr items)))
    (if (null? rest)
        ;; Not (car items) because we want a pair, not value, for end of list. 
        items
        (cons (last items)
              (reverse (all-but-last items))))))

;; Since each last and all-but-last call is O(n) time:
;; time = O(n+n + n-1+n-1 + n-2+n-2 + ... + 1+1) = O(2 * n(n+1)/2) = O(n^2)

;; Luckily, as reverse call stack builds up, all-but-last call stack shrinks,
;; so we don't have O(2n) time:
;; space = O(n)

;; There's lots of repeated traversal of the list. One optimization is to merge
;; last and all-but-last into one procedure to reduce time complexity by a
;; factor of 2, but we still are traversing to the end of the list in each
;; reverse call, so it remains O(n^2) time.

;; Better solution: build new list while traversing:

(define (reverse2 items)
  (define (iter li acc)
    (if (null? li)
        acc
        (iter (cdr li)
              (cons (car li) acc))))
  (iter items nil))

;; Interestingly, in the process of building new list while traversing, the
;; procedure morphed into an iterative process.

;; time = O(n)
;; space = O(n), not from the call stack but from the newly created list.

;; Edit: My inital iterative solution checked (null? (cdr li)), but this fails
;; on empty lists.

;;; Tests

(last (list "A" "B" "C"))
(all-but-last (list 2 4 6 8))
(reverse (list 1 4 9 16 25))
(reverse2 (list 1 4 9 16 25))