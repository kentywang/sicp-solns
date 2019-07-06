#lang racket

;;; Converted to Racket since its Scheme implementation doesn't have delay.

;;; Deps

(define (stream-map proc . argstreams)
  (if (stream-empty? (stream-first argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc
                    (map stream-rest
                         argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define integers
  (stream-cons 1 (stream-map (lambda (x) (+ 1 x))
                             integers)))

(define (print n s)
  (define (recur s c)
    (when (< c n)
      (begin
        (display c)
        (display "  ")
        (display (stream-first s))
        (newline)
        (recur (stream-rest s) (+ c 1)))))
  (recur s 0))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;;; 3.77

(define (integral
         delayed-integrand initial-value dt)
  (stream-cons
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-empty? integrand)
         empty-stream
         (integral
          (delay (stream-rest integrand))
          (+ (* dt (stream-first integrand))
             initial-value)
          dt)))))

;;; Tests

(stream-ref
 (solve (lambda (y) y) 1 0.001) 1000)

;;; 3.78

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

;;; Tests

(stream-ref
 (solve-2nd 2 3 0.001 4 0.02) 100)

;;; 3.79

(define (solve-2nd-general f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;;; Tests

(stream-ref
 (solve-2nd-general + 0.001 5 6) 10)
