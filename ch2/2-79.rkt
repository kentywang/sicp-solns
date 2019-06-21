#lang racket
(require "2-78.rkt")

(define (equ? x y) (apply-generic 'equ? x y))

(define (install-equ-package)
  (put 'equ? '(scheme-number scheme-number) =)
  ;; Can compare simplified against unsimplified forms.
  (put 'equ? '(rational rational) equal?)
  ;; If we just use equal here like for the rationals, we can't compare
  ;; across rectangular/polar forms the internals of the complex package
  ;; doesn't handle it.
  ;; This probably doesn't work well in actuality because of rounding
  ;; inacurracy.
  (put 'equ? '(complex complex)
       (lambda (x y)
         (and (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y))))))

(install-equ-package)

;;; Tests
(equ? 3 3)
(equ? (make-rational 2 3)
      (make-rational 4 6))
(equ? (make-complex-from-real-imag 3.0 4)
      (make-complex-from-real-imag 3 4))