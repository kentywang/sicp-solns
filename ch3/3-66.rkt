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

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define integers
  (cons-stream 1 (stream-map (lambda (x) (+ 1 x))
                             integers)))

;;; Main

(define int-pairs (pairs integers integers))

;; Because of the interleaving, for each increment of the second element of
;; the pair, the stream enumerates once on the recursive call to pairs. So:
;; 1 1 A
;; 1 2 B
;; 2 2 C
;; 1 3 B
;; 2 3 C
;; 1 4 B
;; 3 3 C
;; 1 5 B
;; 2 4 C
;; 1 6 B

;; Therefore, to get to (1, 100), we need to go through 2(n-2) other
;; pairs, plus one for the head.

;; To get to (2, 100), the previous sequence must have been roughly
;; (1, 2*100), so it takes approximately (n-2)*2^2 pairs to get to.

;; To (3, 100), that's ~98*2^2.

;; For (99, 100), that's ~98*2^99.

;; For (100, 100), that's ~98*2^100.

;; Edit: More detailed analysis here:
;; http://community.schemewiki.org/?sicp-ex-3.66
