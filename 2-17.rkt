#lang racket

(define (last-pair a)
  (let ((next (cdr a)))
    (if (null? next)
        a
        (last-pair next))))

(last-pair (list 5))
(last-pair (list 1 2 3))