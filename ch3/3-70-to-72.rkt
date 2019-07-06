#lang sicp

;;; Deps

(define (square x) (* x x))

(define stream-car car)

(define stream-null? null?)

(define (stream-cdr stream)
  (force (cdr stream)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      '()
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

(define (stream-ref s n)
  (if (= n 0)
      (car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (partial-sums s)
  (define ps (add-streams s (cons-stream 0 ps)))
  ps)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))     ; Sₙ₋₁
        (s1 (stream-ref s 1))     ; Sₙ
        (s2 (stream-ref s 2)))    ; Sₙ₊₁
    (cons-stream
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream
   s
   (make-tableau
    transform
    (transform s))))

(define (accelerated-sequence transform s)
  (stream-map car
              (make-tableau transform s)))

(define (stream-limit s tolerance)
  (define (iter s times)
    (let ((next (stream-cdr s)))
     (if (< (abs (- (car s)
                    (car next)))
            tolerance)
         (begin
           (display-line times) ; Added in so I can log.
           (newline)
           (car next))
         (iter next (+ times 1)))))
  (iter s 0))

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

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define integers
  (cons-stream 1 (stream-map (lambda (x) (+ 1 x))
                             integers)))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;;; Main

(define (merge-weighted s1 s2 weight) ; Inputs are streams of list pairs.
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (w1 (apply weight s1car))
                (w2 (apply weight s2car)))
           (cond ((< w1 w2)
                  (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1)
                                   s2
                                   weight)))
                 (else ; Deals with tie case.
                  (cons-stream
                   s2car
                   (merge-weighted s1
                                   (stream-cdr s2)
                                   weight))))))))

(define (weighted-pairs s t weighing-func)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) ; Edit: Changed this to weighted-pairs
                    (stream-cdr t) ; from pairs, since need recursion to
                    weighing-func) ; order the 3rd part. (Otherwise, the
    weighing-func)))               ; inputs to merge-weighted will be off)

;; 1.
(define pairs-by-sum
  (weighted-pairs integers integers (lambda (a b) (+ a b))))

;; 2.
(define pairs-by-hamming
  (let ((non-hamming-ints
          (stream-filter (lambda (element)
                           (not (or (zero? (remainder element 2))
                                    (zero? (remainder element 3))
                                    (zero? (remainder element 5)))))
                         integers)))
    (weighted-pairs non-hamming-ints
                    non-hamming-ints
                    (lambda (a b) (+ (* 2 a)
                                     (* 3 b)
                                     (* 5 a b))))))

;; Helper for testing
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

;;; Tests

(print 20 pairs-by-sum)
(newline)
(print 20 pairs-by-hamming)

;;; 3.71
(define cubic-sum
  (lambda (a b) (+ (expt a 3)
                   (expt b 3))))

(define cubic-sum-pairs
  (weighted-pairs integers integers cubic-sum))

(define ramanujan
  (let ((s cubic-sum-pairs))
    (define (recur t)
      (let ((cs (apply cubic-sum (stream-car t)))
            (next (stream-car (stream-cdr t))))
        (if (= cs (apply cubic-sum next))
            (cons-stream cs
                         (recur (stream-cdr (stream-cdr t))))
            (recur (stream-cdr t)))))
    (recur s)))

;;; Tests
(newline)
(print 6 ramanujan)

;;; 3.72
(define sq-sum
  (lambda (a b) (+ (expt a 2)
                   (expt b 2))))

(define sq-sum-pairs
  (weighted-pairs integers integers sq-sum))

(define trip-sq-sum
  (let ((s sq-sum-pairs))
    (define (recur t)
      (let* ((sqs (apply sq-sum (stream-car t)))
             (next (stream-car (stream-cdr t)))
             (after (stream-car (stream-cdr (stream-cdr t))))
             (next-sum (apply sq-sum next))
             (after-sum (apply sq-sum after)))
        (if (= sqs next-sum after-sum)
            (cons-stream (list (stream-car t) next after)
                         (recur (stream-cdr (stream-cdr (stream-cdr t)))))
            (recur (stream-cdr t)))))
    (recur s)))

;;; Tests
(newline)
(print 5 trip-sq-sum)
