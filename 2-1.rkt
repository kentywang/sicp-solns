#lang sicp

;;; Deps

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;; Main

(define (xor a b)
  (or (and a (not b))
      (and (not a) b)))

(define (make-rat n d)
  (let ((g
         (gcd n d))
        (neg?
         (xor (< n 0) (< d 0))))
    (cons ((if neg? - +)         ; Simplification of (if neg? -1 1).
           (abs (/ n g)))
          (abs (/ d g)))))