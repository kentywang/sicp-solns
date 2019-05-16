#lang racket

;;; Deps

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

;;; 2-7

(define (upper-bound i) (cdr i))

(define (lower-bound i) (car i))

;; Could just do (define lower-bound car).

;;; 2-8

(define (sub-interval x y)
  (make-interval
    (- (lower-bound x)
       (upper-bound y))
    (- (upper-bound x)
       (lower-bound y))))

;;; 2-9

;; Subtraction of two intevals seems to sum the width of the two intervals.
;; Same with addition. Tests:

(define (width interval)
  (- (upper-bound interval)
     (lower-bound interval)))

(define a (make-interval -50 -10))
(define b (make-interval -30 10))
(width a)
(width b)
(width (sub-interval a b))
(width (add-interval a b))
(width (mul-interval a b))
(width (div-interval a b))

;;; 2-10