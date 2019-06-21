#lang sicp

;; 1. Iterative process
(define (cont-frac n d k)

  ;; Build up inner fractions first
  (define (iter i result)
    (if (< i 1)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i)
                    result)))))

  (iter k 0.0))

;; time = O(k)
;; space = O(1)

;; 2. Recursive process
(define (cont-frac-r n d k)

  ;; Build out outer fraction first
  (define (step i)
    (/ (n i)
       (+ (d i)
          (if (> i k)
              0.0
              (step (+ i 1))))))

  (step 1))

;; time = O(k)
;; space = O(k)

;; Both processes need k = 11 to obtain four decimal places of precision.

;; Edit: Embedded "if" in recursive process within the fraction to DRY code.