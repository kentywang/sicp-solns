#lang sicp

;; If we replace the x in the equation
;; f(x) = 1 + 1 / x
;; with the golden ratio, which is defined as
;; φ = (1 + sqrt(5)) / 2
;; then we have
;; 1 + 2 / ((1 + sqrt(5))
;; multiplying the second expression’s numerator and denominator by the
;; conjugate of the denominator
;; = 1 + (2 (1 - sqrt(5)) / ((1 + sqrt(5)(1 - sqrt(5))
;; = 1 + (2 (1 - sqrt(5)) / (1^2 - sqrt(5)^2)
;; = 1 + (2 (1 - sqrt(5)) / (1 - 5)
;; = 1 + (2 (1 - sqrt(5)) / -4
;; = 1 + (2 - 2 * sqrt(5)) / -4
;; multiplying the first expression by -4/-4
;; = (-4 + 2 - 2 * sqrt(5)) / -4
;; = (-2 - 2 * sqrt(5)) / -4
;; = -2 (1 + sqrt(5)) / -4
;; = (1 + sqrt(5)) / 2
;; and so we end up with the original definition for φ, thus showing that
;; f(φ) = φ, and therefore φ is a fixed point for f(x) = 1 + 1 / x.

;;; Deps

(define tolerance 0.0001)

(define (close-enough? a b)
  (< (abs (- a b))
     tolerance))

(define (fixed-point f first-guess)

  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))

  (try first-guess))

;; time = dependent on function f
;; space = O(1)

;;; Main

(define (find-golden-ratio first-guess)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               first-guess))