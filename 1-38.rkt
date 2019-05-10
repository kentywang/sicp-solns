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

;;; Main

(define (euler k)

  (define (denom i)
    (let ((shifted (+ i 1)))
      (if (= (remainder shifted 3) 0)
          (* (/ shifted 3) 2)
          1)))
  
  (cont-frac (lambda (x) 1)
             denom
             k))

;; k: 1  2  3  4  5  6  7  8  9 10 11
;; i: 2  3  4  5  6  7  8  9 10 11 12
;; D: 1  2  1  1  4  1  1  6  1  1  8

;; k=1: 1/1 = 1.0 
;; k=2: 1/(1+(1/2)) = 0.67
;; k=3: 1/(1+(1/(2+(1/1))) = 0.75