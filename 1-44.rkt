#lang sicp

;;; Deps

(define (square x) (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter count composed-func)
    (if (= count n)
        composed-func
        (iter (+ count 1) (compose f composed-func))))
  (iter 1 f))

;;; Main

(define (average f g h)
  (lambda (x)
    (/ (+ (f x)
          (g x)
          (h x))
       3)))

(define (smooth f)
  (let ((dx 0.0001))
    (average f
             (lambda (x) (f (- x dx)))
             (lambda (x) (f (+ x dx))))))

;;; Test

((smooth square) 2)
(((repeated smooth 15) square) 2)