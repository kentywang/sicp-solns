#lang sicp

;;; 2.38

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op 
                      initial 
                      (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)) ; (/ 1 (/ 2 (/ 3 1))), or 3/2
(fold-left  / 1 (list 1 2 3)) ; (/ (/ (/ 1 1) 2) 3), or 1/6
(fold-right list nil (list 1 2 3)) ; (1 (2 (3 ()))) [Edit: forgot the ()]
(fold-left  list nil (list 1 2 3)) ; (((() 1) 2) 3) [Same]

;; op must satisfy the commutative property, that is, op(a, b) = op(b, a), in
;; order for fold-left and fold-right to produce the same value.

;; Edit: And also associative (which is why multiplication works but not division).

;;; 2.39

;; Helper
(define (last items)
  (and (not (null? items))
       (let ((next (cdr items)))
         (if (null? next)
             (car items)
             (last next)))))

(define (reverse sequence)
  (fold-right 
   (lambda (x y) (cond ((null? y) (cons x y))
                       ((pair? y) (append y (list x)))))
  nil sequence))

;; time: O(n^2)
;; space: O(n)

(define (reverse2 sequence)
  (fold-left
   (lambda (x y) (cons y x)) nil sequence))

;; time: O(n)
;; space: O(n), or O(1) if we don't count the new list created.

(reverse '())
(reverse2 '())
(reverse '(1))
(reverse2 '(1))
(reverse '(4 5 6))
(reverse2 '(4 5 6))
