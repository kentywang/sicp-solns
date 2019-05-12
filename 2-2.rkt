#lang racket
(provide (all-defined-out))

;;; Utils

(define (average x y)
  (/ (+ x y) 2))

;;; Line segment

(define (make-segment a b)
  (cons a b))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cdr l))

;;; Point

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

;;; The next layer up, which uses points and segments

(define (midpoint-segment l)
  (let ((start (start-segment l))
        (end (end-segment l)))
    (make-point (average (x-point start)
                         (x-point end))
                (average (y-point start)
                         (y-point end)))))

;;; For testing

;; Given to us
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-mid a b x y)
  (print-point (midpoint-segment
                 (make-segment (make-point a b)
                               (make-point x y)))))
                                         