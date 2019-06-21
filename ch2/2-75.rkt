#lang sicp

;;; Deps

(define (apply-generic op arg) (arg op))

(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

;;; Main

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op" op))))
  dispatch)

;;; Test

(define c (make-from-mag-ang 3 4))
(real-part c)
(imag-part c)
(magnitude c)
(angle c)