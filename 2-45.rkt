#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (split t1 t2)
  (define (recur painter n)
    (if (= n 0)
        painter
        (let ((smaller (recur painter (- n 1))))
          (t1 painter
              (t2 smaller smaller)))))
  recur)

(define right-split (split beside below))
(define up-split (split below beside))

;;; Tests

(paint (up-split einstein 2))
(paint (right-split einstein 2))