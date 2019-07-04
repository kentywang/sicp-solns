#lang racket

;; 3.59

;;; Deps

(define (integers-starting-from n)
  (stream-cons
   n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc 
                    (map stream-rest
                         argstreams))))))

(define (enum s times)
  (if (> times 0)
      (begin (display (stream-first s))
             (newline)
             (enum (stream-rest s) (- times 1)))
      'done))

(define fives (stream-cons 5 fives))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

;;; Main 1.

(define (div-streams a b)
  (stream-map / a b))

(define (integrate-series a)
  (div-streams a integers))

;; a0 a1 a2 a4
;; 1  2  3  4

;;; Tests

(enum (integrate-series fives) 10)

(define exp-series
  (stream-cons 
   1 (integrate-series exp-series)))
(enum exp-series 10)

;;    1   2   3   4    5
;; 1 1/1 1/2 1/6 1/24 1/120

;;; Main 2.

(define cosine-series 
  (stream-cons 1 (scale-stream (integrate-series sine-series)
                               -1)))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

;;; Tests

(enum cosine-series 10)
(enum sine-series 10)