#lang sicp

;; Helpers
(define (square x) (* x x))

;; Main
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Test
((compose square inc) 6)