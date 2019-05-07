#lang sicp

;; Deps

(define square
  (lambda (n)
    (* n n)))

;; Main

(define (non-trivial-sqrt? x n)
  (and (not (= x 1))
       (not (= x (- n 1)))
       (= (remainder (square x)
                     n)
          1)))

(define (expmod base exp m)
  (define (check x)
    (if (non-trivial-sqrt? x m)
        0
        x))
  
  (cond ((= exp 0) 1)
        ((even? exp)            
         (remainder 
          (square (check (expmod base (/ exp 2) m)))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (milner-rabin-test n)  
  (define (iter times)
    (cond ((= times 0)
           #t)
          ((= (expmod times
                      (- n 1)
                      n)
              1)
           (iter (- times 1)))
          (else
           #f)))

  (iter (- n 1)))

;; Help from http://jots-jottings.blogspot.com/2011/09/sicp-128-miller-rabin-test.html