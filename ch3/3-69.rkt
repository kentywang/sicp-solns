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

(define tripint
  (let ((t (pairs (pairs integers integers)
                  integers))) ; k >= j, so need inner pair as 1st arg
    (define (recur s)
      (cons-stream (append (car (stream-car s)) ; Spreads the inner list.
                           (cdr (stream-car s)))
                   (recur (stream-cdr s))))
    (recur t)))

;; Problem with this implementation: It tries to prime the first element,
;; and the search is long because the order is not well-constructed. It
;; goes through k quickly while i and j barely budge.

;; (define pyth-trips
;;   (stream-filter (lambda (e)
;;                    (display e)
;;                    (newline)
;;                    (= (square (caddr e))
;;                       (+ (square (car e))
;;                          (square (cadr e)))))
;;                  tripint))

;;; Tests

(stream-ref tripint 0)
(stream-ref tripint 1)
(stream-ref tripint 2)
;; (stream-ref tripint 3)
;; (stream-ref tripint 4)
;; (stream-ref tripint 5)
;; (stream-ref tripint 6)
;; (stream-ref tripint 7)
;; (stream-ref tripint 8)
;; (stream-ref tripint 9)
;; (stream-ref tripint 10)

;; (stream-ref pyth-trips 0)
;; (stream-ref pyth-trips 1)
;; (stream-ref pyth-trips 2)
;; (stream-ref pyth-trips 3)
;; (stream-ref pyth-trips 4)
;; (stream-ref pyth-trips 5)
;; (stream-ref pyth-trips 6)
;; (stream-ref pyth-trips 7)
;; (stream-ref pyth-trips 8)
;; (stream-ref pyth-trips 9)
;; (stream-ref pyth-trips 10)


