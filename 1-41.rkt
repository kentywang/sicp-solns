#lang sicp

(define (double f)
  (lambda (x)
    (f (f x))))

;; (((double (double double)) inc) 5) should call inc 8 times, so it would
;; return 13.

;; Edit: wrong, it calls inc 16 times

((double inc) 5)
;; ↓
(((lambda (f)
    (lambda (x)
      (f (f x)))) inc) 5)
;; ↓
((lambda (x)
   (inc (inc x))) 5)
;; ↓
(inc (inc 5))

;; (((double (double double)) inc) 5)
;; ↓
(((double ((lambda (f)
             (lambda (x)
               (f (f x)))) double)) inc) 5)
;; ↓
(((double (lambda (x)
            (double (double x)))) inc) 5)
;; ↓
((((lambda (f)
     (lambda (x)
       (f (f x))))
   (lambda (x)
     (double (double x)))) inc) 5)
;; ↓
(define (dd x) (double (double x)))
  
(((lambda (x)
     (dd (dd x))) inc) 5)
;; ↓
((dd (dd inc)) 5)

;; 1 double:  calls f twice
;; 2 doubles: calls double on double, so f called 2^2 = 4 times
;; 3 doubles: calls dd on dd, so f called 2^2^2 = 16 times