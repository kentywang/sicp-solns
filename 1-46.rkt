#lang sicp

;;; Deps

(define square (lambda (x) (* x x)))

(define (average x y) 
  (/ (+ x y) 2))

;;; Main

(define (iterative-improve stop improve)
  (define (iter guess)
    (if (stop guess)
        guess
        (iter (improve guess))))
  (lambda (start)
    (iter start)))

;; time = dependent on stop and improve functions
;; space = O(1)

(define (sqrt x)

  (define (good-enough? guess)
    (< (abs (- x (square guess))) 0.0001))

  (define (improve guess)
    (average guess (/ x guess)))
             
  ((iterative-improve good-enough?
                      improve)
   1.0))

(define (fixed-point f x)
  
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) 0.0001))

  ((iterative-improve good-enough?
                     f)
   x))