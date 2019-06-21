#lang sicp

;;; Deps

(define (square x) (* x x))

(define (square-tree tree) 
  (tree-map square tree))

;;; Main

(define (tree-map f t)
  (map (lambda (n)
         (if (pair? n) (tree-map f n) (f n)))
       t))

;;; Tests

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))