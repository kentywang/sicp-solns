#lang sicp

;;; Deps

(define (square x) (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

;;; Main

;; Recursive process
(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ;; Need compose instead of f(f(x)) since we need to return a function,
        ;; not a function call, in order to pass x in.
        ((compose f
                  (repeated f (- n 1)))
           x))))

;; time = O(n)
;; space = O(n)

;; Iterative
(define (repeated-i f n)
  (define (iter count composed-func)
    (if (= count n)
        composed-func
        (iter (+ count 1) (compose f composed-func))))
  
  (iter 1 f))

;; time = O(n)
;; space = O(1)

;; Test
((repeated square 2) 5)
((repeated-i square 2) 5)