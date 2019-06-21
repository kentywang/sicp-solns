#lang sicp

;; 1.
(define (accumulate combiner
                    null-value
                    term
                    a
                    next
                    b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner
                            null-value
                            term
                            (next a)
                            next
                            b))))

;; 2.
(define (accumulate-2 combiner
                      null-value
                      term
                      a
                      next
                      b)
  
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result
                                 (term a)))))

  (iter a null-value))

;;; New sum and product procedures

(define (sum term a next b)

  (define (add x y)
    (+ x y))
  
  (accumulate-2 add
                0
                term
                a
                next
                b))

(define (product term a next b)

  (define (multiply x y)
    (* x y))
  
  (accumulate-2 multiply
                1
                term
                a
                next
                b))

;;; Test procedures

(define (sum-integers a b)
  (sum identity a inc b))

(define (factorial n)  
  (product identity 1 inc n))

;;; Helpers

(define (identity x) x)

(define (inc n) (+ n 1))

;; Post-solution edit: I didn’t need to define add and multiply, since + and *
;; would have worked with the combiner.