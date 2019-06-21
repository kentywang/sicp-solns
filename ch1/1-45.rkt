#lang sicp

;;; Deps

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (expt x n)
  (define (iter n a b)
    (cond ((= n 1)
           (* a b))
          ((even? n)
           (iter (/ n 2) (* a a) b))
          (else
           (iter (- n 1) a (* a b)))))
  (if (= n 0)
      1
      (iter n x 1)))

(define (repeated f n)
  (define (iter count a b)
    (cond ((= count 0)
           (compose a b))
          ((even? count)
           (iter (/ count 2) (compose a a) b))
          (else
           (iter (- count 1) a (compose f b)))))
  (iter n f identity))

;;; Main

(define (nth-root n x times)
  
  (fixed-point ((repeated average-damp times)
                  (lambda (y)
                    (/ x (expt y (- n 1)))))
               1.0))

(define (nth-root-go n x)
  (nth-root n
            x
            (floor (- (log n 2)
                      1))))

;;; Tests

;; > (nth-root 2 4 0)
;; 2.000000000000002
;; > (nth-root 3 8 0)
;; 1.9999981824788517
;; > (nth-root 4 16 0)
;; Doesn’t terminate
;; > (nth-root 4 16 1)
;; 1.844363126744897, why is it off?

;; Interestingly, (nth-root 5 32 2) and (nth-root 5 32 4) don’t terminate when
;; I use the fast-repeated, and (nth-root 5 32 1) and (nth-root 5 32 3) don’t
;; when I use the O(n) repeated.

;; Edit: I was repeating average-damp(f(y)) instead of average-damp itself,
;; which was causing issues.

;; Now with the fix, observations on how many repeats needed:
;; Repeats  nth-root
;; 0        2
;; 1        4
;; 2        8
;; 3        16
;; This shows the repeat count must be log2(n) + 1.