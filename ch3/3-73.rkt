#lang sicp

;;; Deps

(define stream-car car)

(define stream-null? null?)

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      '()
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

(define (stream-for-each proc s)
  (if (null? s)
      'done
      (begin
        (proc (car s))
        (stream-for-each proc
                         (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter 
           pred
           (stream-cdr stream))))
        (else (stream-filter 
               pred 
               (stream-cdr stream)))))

(define (stream-ref s n)
  (if (= n 0)
      (car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define integers
  (cons-stream 1 (stream-map (lambda (x) (+ 1 x))
                             integers)))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

(define (print n s)
  (define (recur s c)
    (if (< c n)
        (begin
          (display c)
          (display "  ")
          (display (stream-car s))
          (newline)
          (recur (stream-cdr s) (+ c 1)))))
  (recur s 0))

;;; Main

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C))
                           v0
                           dt))))

;;; Tests

(define RC1 (RC 5 1 0.5))
(print 10 (RC1 integers 3))
