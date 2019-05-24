#lang sicp

;;; Main

(define (square-tree tree)
  (map (lambda (item)
         (if (pair? item)
             (square-tree item)
             (* item item)))
       tree))

(define (square-tree2 tree)
  (cond ((null? tree)
         nil)
        ((pair? tree)
         (cons (square-tree2 (car tree))
               (square-tree2 (cdr tree))))
        (else
         (* tree tree))))

;;; Tests

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))