#lang sicp

;;; Deps

(define (square x) (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

;;; Main

;; Recursive process
(define (repeated f n)
  (if (= n 1)
      f
      ;; Need compose instead of f(f(x)) since we need to return a function,
      ;; not a function call, in order to pass x in.
      (compose f
               (repeated f (- n 1)))))

;; time = O(n)
;; space = O(n)

;; Iterative
(define (repeated-i f n)
  
  (define (iter count composed-func)
    (if (= count n)
        composed-func
        (iter (+ count 1) (compose f composed-func))))
  
  (iter 1 f))

;; time = O(n)
;; space = O(1)

;;; Tests

((repeated square 2) 5)
((repeated-i square 2) 5)

;; Edit: My original solution was this:
(define (repeated-o f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ;; Need compose instead of f(f(x)) since we need to return a function,
        ;; not a function call, in order to pass x in.
        ((compose f
                  (repeated-o f (- n 1)))
           x))))
;; It needed to be DRYed more since this is the same issue as
;; ajax(json => callback(json)), where I could just do ajax(callback).
;; I just need to realize that if the return of the lambda applies an operator
;; to the arguments, I could just return the operator itself.

;; Also, the iterative process is probably not O(1) space (and still O(n),
;; since the composition builds up, so the “shape” of the process is not
;; consistent.
;; You can improve the time complexity of both types of processes to O(log n)
;; by simplifying when n is even to (repeated (compose f f) (/ n 2))).

;; With that idea, here’s my O(log n) time iterative solution:
(define (fast-repeated f n)
  (define (iter count a b)
    (cond ((= count 0)
           (compose a b))                        ; Return a added with b.
          ((even? count)
           (iter (/ count 2) (compose a a) b))   ; Double a.
          (else
           (iter (- count 1) a (compose f b))))) ; Add an f onto b.
  (iter n f identity))

;; Comparision with analogous fast-expt-iter:
;; Exp        n   a   b
;; 2^7        7   
;; 2^6 * 2    6       1
;; 4^3 * 2    3   2   1
;; 4^2 * 8    2   2   2
;; 16  * 8    1   4   2
;; 128        0   4   3
;; Total steps: 5 (instead of 7)