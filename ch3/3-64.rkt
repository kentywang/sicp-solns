#lang sicp

;;; Deps

(define (stream-cdr stream)
  (force (cdr stream)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (stream-map proc s)
  (if (null? s)
      '()
      (cons-stream
       (proc (car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-ref s n)
  (if (= n 0)
      (car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;;; Main

(define (stream-limit s tolerance)
  (let ((next (stream-cdr s)))
    (if (< (abs (- (car s)
                   (car next)))
           tolerance)
        (car next)
        (stream-limit next tolerance))))

;;; Tests

(sqrt 2 0.1)
(sqrt 2 0.01)
(sqrt 2 0.0000001)
