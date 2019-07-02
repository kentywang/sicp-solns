#lang sicp

;;; Deps

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream 
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

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

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

(define stream-null? null?)

;;; Main

(define sum 0)
;; sum = 0

(define (accum x)
  (set! sum (+ x sum))
  sum)
;; sum = 0

(define seq
  (stream-map
   accum
   (stream-enumerate-interval 1 20)))
;; seq = (cons-stream 1 (stream-map accum (stream-enumerate-interval 2 20)))
;; sum = 1


(define y (stream-filter even? seq))
;; y = (cons-stream 6 (stream-map accum (stream-enumerate-interval 4 20)))
;; sum = 6

(define z
  (stream-filter
   (lambda (x)
     (= (remainder x 5) 0)) seq))
;; z = (cons-stream 15 (stream-map accum (stream-enumerate-interval 5 20)))
;; sum = 15
;; If memoized, then sum = 10, z = (cons-stream 10 <...>)

(stream-ref y 7)
;; (cons-stream 162 (stream-map accum (stream-enumerate-interval 18 20)))
;; sum = 162
(display-stream z)
;;
;; 15
;; 180 (167, 173 not printed since not divisible by 5)
;; ...
;; done
;; If memoized, would be 10, 15, 45, etc.
