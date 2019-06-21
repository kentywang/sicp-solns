#lang sicp

;;; Utility procedures

(define square (lambda (x) (* x x)))

;;; Prime-finding procedures

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

;;; Testing procedures

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 3)
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

;; Expect when input size grows from 1,000 to 1,000,000, time increases by
;; log2(1e6)/log2(1e3) = 2. (I can’t just look at one log2(n), I have to
;; compare.)

;; 1009 *** 8
;; 1013 *** 8
;; 1019 *** 8

;; 10007 *** 9
;; 10009 *** 9
;; 10037 *** 10

;; 100003 *** 12
;; 100019 *** 10
;; 100043 *** 12

;; 1000003 *** 13
;; 1000033 *** 13
;; 1000037 *** 13

;; In practice, larger inputs actually run faster than expected. Actual: 13/8
;; is a 1.623 time increase, instead of 2. It makes more sense if we factor in
;; a baseline runtime of 4-5 ms, so that the ratio is then 8/3 or 9/4, making
;; it slightly slower than the ideal, and thus a realistic answer.