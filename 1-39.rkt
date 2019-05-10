#lang sicp

;;; Deps

(define (cont-frac n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i)
                    result)))))
  (iter k 0.0))

;;: Main

(define (tan-cf x k)

  (define (numer i)
    (if (= 1 i)
        x
        (- (* x x))))

  (define (denom i)
    (- (* i 2) 1))

  (cont-frac numer denom k))