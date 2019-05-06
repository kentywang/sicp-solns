#lang sicp

(define (smallest-divisor n)
  (find-divisor n 2))
(define square (lambda (x) (* x x)))
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

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) 
                       start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end) 
  (iter
    (if (= 0 (remainder start 2))    ; If even, start with the next integer,
        (+ start 1)                  ; which is odd.
        start)
    end))

(define (iter current end)
  (cond ((<= current end)
         (timed-prime-test current)
         (iter (+ current 2) end)))) ; We only want to check odd integers.

;; We expect when input size by 10, then time increases by sqrt(10), or
;; approx 3.16.

;; 1009 *** 4
;; 1013 *** 4
;; 1019 *** 4

;; 10007 *** 11
;; 10009 *** 11
;; 10037 *** 10

;; 100003 *** 32
;; 100019 *** 30
;; 100019 *** 30

;; 1000003 *** 97
;; 1000033 *** 97
;; 1000037 *** 97

;; And experimental results fall in line with those expectations.