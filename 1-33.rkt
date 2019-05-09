#lang sicp

;;; Setup

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (identity x) x)

;;; Main

(define (filtered-accumulate combiner
                             predicate
                             null-value
                             term
                             a
                             next
                             b)
  
  (define (iter a result)

    ;; Logging
    (display "a: ")
    (display a)
    (display ", result: ")
    (display result)
    (newline)
    
    (if (> a b)
        result
        (iter (next a)
              (combiner result
                        (if (predicate a)
                            (term a)
                            null-value)))))

  (iter a null-value))

;; 1.
(define (sum-squared-primes a b)
  (filtered-accumulate +
                       prime?
                       0
                       square
                       a
                       inc
                       b))

;; 2.
(define (prod-rel-primes n)
  
  (define (rel-prime? i)
    (= 1 (gcd i n)))
    
  (filtered-accumulate *
                       rel-prime?
                       1
                       identity
                       1
                       inc
                       (- n 1)))
