#lang sicp

((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

;; 1. An iterative O(n) approach is actually easier than recursive.
(define (fib n)
  ((lambda (f) (f f n 0 1))
   (lambda (g o p q)
     (if (= o 0)
         q
         (g g (- o 1) q (+ p q))))))
         
;; 2. Interesting solution! We need to maintain the same function
;; signatures and recursive arguments between the two functions so we
;; can interleave their calls. No need to worry about references being
;; defined yet, since we pass the references to the procedure to each
;; subsequent recursion.
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) 
         true 
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) 
         false 
         (ev? ev? od? (- n 1))))))