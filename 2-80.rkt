#lang racket
(require "2-78.rkt")

(define (=zero? y) (apply-generic '=zero? y))

(define (install-zero-package)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  ;; Can compare simplified against unsimplified forms.
  (put '=zero? '(rational)
       (lambda (x)
         (= (numer x) 0)))
  ;; If we just use equal here like for the rationals, we can't compare
  ;; across rectangular/polar forms the internals of the complex package
  ;; doesn't handle it.
  ;; This probably doesn't work well in actuality because of rounding
  ;; inacurracy.
  (put '=zero? '(complex)
       (lambda (x)
         (and (= (real-part x) 0)
              (= (imag-part x) 0)))))

(install-zero-package)

;;; Tests
(=zero? 0)
(=zero? 3)
(=zero? (make-rational 0 3))
(=zero? (make-rational 1 3))
(=zero? (make-complex-from-mag-ang 0 1))
(=zero? (make-complex-from-mag-ang 1 1))