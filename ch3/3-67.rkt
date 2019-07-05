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

(define (pairst s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list x (stream-car t))) ;; Just change this.
                (stream-cdr s)) ;; And this.
    (pairst (stream-cdr s) (stream-cdr t)))))

(define (full-pairs s t)
  (interleave (pairs s t)
              (pairst (stream-cdr s) t))) ;; Skip the first row.

(define all-int-pairs (full-pairs integers integers))

;; Trying to obtain the transpose as (pairs (stream-cdr t) s)) doesn't
;; work, since all it does is translate the starting point.

;;; Tests

(stream-ref all-int-pairs 0)
(stream-ref all-int-pairs 1)
(stream-ref all-int-pairs 2)
(stream-ref all-int-pairs 3)
(stream-ref all-int-pairs 4)

;;; Edit: Can combine in single procedure!
(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (all-pairs (stream-cdr s) (stream-cdr t)))
    (stream-map (lambda (x) (list x (stream-car t)))
                (stream-cdr s)))))
