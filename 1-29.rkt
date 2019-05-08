#lang sicp

;;; Deps

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))

;;; Main

(define (simpson f a b n)
  
  (define h
    (/ (- b a) n))

  (define (wrap x)
    (* (coeff (/ (- x a) h)) ; Obtain k from a - kh.
       (f x)))
  
  (define (coeff k)
    (cond ((or (= k 0)
               (= k n))
           1)
          ((even? k)
           2)
          (else
           4)))
  
  (define (next x)
    (+ x h))
  
  (* (/ h 3.0)
     (sum wrap a next (+ a (* n h)))))

;; time = O(b-a)
;; space = O(b-a)

;; This procedure is more accurate. With n = 2:
;; > (simpson cube 0 1 2)
;; 0.25

;; Online solutions have a better approach: sum g(k) from 0 to n, where
;; g = coeff(k) * f(a + kh).
