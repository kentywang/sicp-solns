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

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))

(define upper-bound cdr)

(define lower-bound car)

(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (percent x)
  (/ (width x) (center x)))

(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

;;; 2-14: Tests

(define a (make-interval 100 105))
(define b (make-interval 4 5))

(par1 a b)
(par2 a b)

(define div-a-a (div-interval a a))
(define div-a-b (div-interval a b))

(center div-a-a)
(percent div-a-a)
(center div-a-b)
(percent div-a-b)

;;; 2-15

;; I don't think Alyssa is right. Maybe just so happens that it's very obvious
;; when you look at A/A to see the error in the calculation, but it doesn't
;; mean that avoiding duplicate variables is less errorneous.

;;; 2-16

;; Equivalent algebraic expressions can generate different answers because the
;; underlying data structures representing them use imprecise floating point
;; arithmetic, and different structures employ different types of arithmetic,
;; leading to discrepancies.

;; lol

;; Edit: As always: http://community.schemewiki.org/?sicp-ex-2.14-2.15-2.16