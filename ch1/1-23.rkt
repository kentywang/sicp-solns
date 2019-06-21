#lang sicp

;;; Utility procedures

(define square (lambda (x) (* x x)))

(define (divides? a b)
  (= (remainder b a) 0))

;;; Testing procedures

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

;;; Main procedures

(define (smallest-divisor n)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) 
           n)
          ((divides? test-divisor n) 
           test-divisor)
          (else (find-divisor (next test-divisor)))))
  
  (define (next n) ; Expects inputs of 2, 3, 5, 7, etc.
    (if (= n 2)
        3
        (+ n 2)))

  (find-divisor 2))

(define (prime? n)
  (= n (smallest-divisor n)))

;; 1009 *** 3
;; 1013 *** 3
;; 1019 *** 3

;; 10007 *** 7
;; 10009 *** 8
;; 10037 *** 7

;; 100003 *** 19
;; 100019 *** 19
;; 100043 *** 19

;; 1000003 *** 59
;; 1000033 *** 60
;; 1000037 *** 59

;; So removing the even numbers for each prime check seems to decrease the
;; runtime to 60% of its original value.

;; That’s pretty close to matching the expectations of halving the runtime.
;; It's not as fast maybe because of the if check in the next procedure now
;; needed to run in each iteration, or that calling the next procedure in
;; itself incurs a runtime penalty over, say, simply having the if within
;; find-divisor.



