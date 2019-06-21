#lang sicp

;; 1. Recursive process version
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; time = O((b - a) / term(a))
;; space =  O((b - a) / term(a))

;; 2. Iterative process version
(define (product-2 term a next b)

  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))

  (iter a 1))

;; time = O((b - a) / term(a))
;; space =  O(1)

;;; Helpers

(define (identity x) x)

(define (inc x)
  (+ x 1))

;;; Procedures using the product procedure

(define (factorial n)  
  (product identity 1 inc n))

(define (pi n)

  (define (fraction x)
    (if (even? x)
        (/ x (inc x))
        (/ (inc x) x)))
  
  (* 4.0
     (product fraction 2 inc n)))