#lang racket

;; a b cons
;; 0 0 1
;; 0 1 3
;; 0 2 9
;; 0 3 27

;; 1 0 2
;; 1 1 6
;; 1 2 18
;; 1 3 54

;; 2 0 4
;; 2 1 12
;; 2 2 36
;; 2 3 108

;; 3 0 8
;; 3 1 24
;; 3 2 72
;; 3 3 216

;; Observations: If cons is a number n such that n = 2^x for integer x > 1,
;; then the cdr is 0. Likewise, if it’s n = 3^x for integer x > 1, then the car
;; is 0.

;; If cons is 1, then both car and cdr are 0.

;; I assume that there is only one possible car and cdr for any given cons.

;; Note that cons need not to be able to assume any positive integer value. It
;; just needs to hold a positive integer value that can _map_ to any pair of
;; positive integer values.

;; But back to the observation. We know that if cons is not 1 or an exponential
;; of 2 or 3, then the car and the cdr are both greater than 0, so we can
;; reduce our problem down.

;;; Helpers

(define (expt? b prod)
  (= (expt b                    
           (floor (log prod b)))
     prod))

;;; Main

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car c)
  (define (iter prod a)
    (cond ((expt? 3 prod)                  ; a is 0, so return.
           a)
          ((expt? 2 prod)
           (log prod 2)) ; Could DRY this
          (else
           (iter (/ prod 2) (+ a 1)))))    ; Reduce prod by 2 and try again.
  (iter c 0))

;; time = O(a) [worst case, happens when a = b]
;; space = O(1)

(define (cdr c)
  (define (iter prod b)
    (cond ((expt? 2 prod)
           b)
          ((expt? 3 prod)
           (log prod 3))
          (else
           (iter (/ prod 3) (+ b 1)))))
  (iter c 0))

;;; Tests

(define z (cons 1 2))
(car z)
(cdr z)

(define w (cons 11 7))
(car w)
(cdr w)