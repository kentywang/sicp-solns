#lang sicp

(define (f g) (g 2))

;; is actually

(define f
  (lambda (g)
    (g 2)))

;; so (f f) is

((lambda (g) (g 2)) (lambda (g) (g 2)))

;; and using the substitution method, we get

((lambda (g) (g 2)) 2)

;; applying substitution again, we get

(2 2)

;; which throws an error because 2 is not a procedure.