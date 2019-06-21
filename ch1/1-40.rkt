#lang sicp

;;; Deps

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) 
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) 
               guess))

;;; Main

(define (cubic a b c)
  (lambda (x)
    (let ((x2 (* x x)))   
      (+ (* x x2)
         (* a x2)
         (* b x)
         c))))

;; > (newtons-method (cubic 1 2 3) 1)
;; -1.2756822036498454