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

(define (div-interval-2 x y)
  (let ((upper-y (upper-bound y))
        (lower-y (lower-bound y)))
    (if (or (= 0 upper-y)
            (= 0 lower-y))
        (error "Bound cannot be 0, since division by 0 is undefined." y)
        (mul-interval x 
                      (make-interval 
                       (/ 1.0 (upper-bound y)) 
                       (/ 1.0 (lower-bound y)))))))

;; Test
(define c (make-interval 0 1))
; (div-interval-2 a c)

;;; 2-11

(define (mul-interval-2 x y)
  (let ((upper-x (upper-bound x))
        (lower-x (lower-bound x))
        (upper-y (upper-bound y))
        (lower-y (lower-bound y)))
    (cond ((and (> lower-x 0)                   ; 1. ++ ++
                (> upper-x 0)
                (> lower-y 0)
                (> upper-y 0))
           (make-interval (* lower-x lower-y)
                          (* upper-x upper-y)))
          ((and (< lower-x 0)                   ; 2. -+ ++
                (> upper-x 0)
                (> lower-y 0)
                (> upper-y 0))
           (make-interval (* lower-x upper-y)
                          (* upper-x upper-y)))
          ((and (< lower-x 0)                   ; 3. -- ++
                (< upper-x 0)
                (> lower-y 0)
                (> upper-y 0))
           (make-interval (* lower-x upper-y)
                          (* upper-x lower-y)))
          ((and (< lower-x 0)                   ; 4. -- -+
                (< upper-x 0)
                (< lower-y 0)
                (> upper-y 0))
           (make-interval (* lower-x upper-y)
                          (* upper-x lower-y)))
          ((and (< lower-x 0)                   ; 5. -- --
                (< upper-x 0)
                (< lower-y 0)
                (< upper-y 0))
           (make-interval (* lower-x lower-y)
                          (* upper-x upper-y)))
          ((and (< lower-x 0)                   ; 6. -+ -+
                (> upper-x 0)
                (< lower-y 0)
                (> upper-y 0))
           (make-interval (min (* lower-x upper-y)
                               (* upper-x lower-y))
                          (max (* lower-x lower-y)
                               (* upper-x upper-y))))
          (else                                 ; Mirror cases for 2, 3, 4
           (mul-interval-2 y x)))))

;; Test
(mul-interval-2 a b)

;;; 2-12

(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (percent x)
  (/ (width x) (center x)))

;; Not the most robust (can't handle zero centers).

;;; 2-13

;; Small percent tolerances are additive during interval multiplication:
;; 10 10% = 10 1w = 9 - 11
;; 4 25% = 4 1w = 3 - 5
;; Product
;; 27 - 55 = 41 14w = 41 ~34.5%
;; Notice that center of interval isn't 40, showing that interval;
;; multiplication doesn't result in multiplicative centers.

