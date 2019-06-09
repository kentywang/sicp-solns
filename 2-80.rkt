#lang racket
(require "2-78.rkt")

(define (=zero? y) (apply-generic '=zero? y))

(define (install-zero-package)
  ;; (zero? x) is like (= x 0)
  (put '=zero? '(scheme-number) zero?)
  (put '=zero? '(rational) (lambda (x) (zero? (numer x))))
  (put '=zero? '(complex) (lambda (x) (zero? (magnitude x)))))

(install-zero-package)

;;; Tests
(=zero? 0)
(=zero? 3)
(=zero? (make-rational 0 3))
(=zero? (make-rational 1 3))
(=zero? (make-complex-from-mag-ang 0 1))
(=zero? (make-complex-from-mag-ang 1 1))