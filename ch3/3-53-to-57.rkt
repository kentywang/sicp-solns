#lang sicp

;; 3.53
;; 1, 2, 4, 8, 16, ...

;; 3.54
(define factorials 
  (cons-stream 1 (mul-streams (integers-starting-from 2) factorials)))

(define (mul-streams x y)
  (stream-map * x y))

;;  1 2 6  24  120 720
;;  2 3 4  5   6   7
;;  2 6 24 120 720

;; 3.55
(define (partial-sums s)
  (cons-stream (stream-car s) (add-stream (stream-car s) (partial-sum s))))

;; 1 2 3 4
;; 0 1 3 6
;; 1 3 6 10

;; Can be optimized via self-ref:

(define (partial-sums s) 
  (define ps (add-streams s (cons-stream 0 ps))) 
  ps) 

;; 3.56
(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5))))

;; 3.57
;; There would be n-2 additions to calculate the nth fib. Without memorization, each
;; subsequent element is the addition of two fib streams, each of which spawn an
;; additonal addition calls. So the number of additions doubles. So 2^n additions[1].
;; [1] Actually 2^1.618.