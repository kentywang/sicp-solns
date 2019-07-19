#lang sicp

;;; Deps

(define (require p)
  (if (not p) (amb)))

;;; Main

(define (an-integer-between a b)
  (require (<= a b))
  (amb a
       (an-integer-between (+ a 1) b)))

;;; Test

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) 
                    (* k k)))
        (list i j k)))))

;; Still trying to understand amb logic.
;; These need to run on separate instances, otherwise they interfere with each other.

;(a-pythagorean-triple-between 1 10)
;(a-pythagorean-triple-between 8 20)
;(a-pythagorean-triple-between 20 20)