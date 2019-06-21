#lang racket

(define (cons x y) 
  (lambda (m) (m x y)))

(define (car z) 
  (z (lambda (p q) p)))

;; Using the substitution method:
(car (cons 1 2))
;; becomes
(car (lambda (m) (m 1 2)))
;; becomes
((lambda (m) (m 1 2)) (lambda (p q) p))
;; becomes
((lambda (p q) p) 1 2)
;; becomes
1

;; The definition of cdr can be similarly shown.

(define (cdr z)
  (z (lambda (p q) q)))