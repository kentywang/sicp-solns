#lang sicp

;;; Deps

(define (square x) (* x x))

(define (identity x) x)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter count a b)
    (cond ((= count 0)
           (compose a b))
          ((even? count)
           (iter (/ count 2) (compose a a) b))
          (else
           (iter (- count 1) a (compose f b)))))
  (iter n f identity))

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

((average (lambda (x) x)
          (lambda (x) (* x 2))
          (lambda (x) (* x 3)))
    1)
((smooth square) 2)
((smooth (lambda (x) (* x 5))) 2)
(((repeated smooth 15) square) 2)