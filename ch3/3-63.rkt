#lang sicp

;; Without the local variable, enumerating just the nth element of
;; (sqrt-stream x) would require recomputation of the 1st element, 2nd element,
;; ..., nth element, meaning there would be n(n+1)/2 operations to enumerate
;; from the 1st to the nth element. I believe the space complexity grows also
;; quaduatically with respect to n, since a stack of sqrt-streams is formed.

;; With the local variable, the reference to the original stream is passed
;; along, so space complexity is kept linear to the length of the sequence.

;; Even with local variables, however if there is no memoization, the
;; recompulation still occurs, so there's O(n^2) time complexity. Memoization
;; allows each enumeration operation to cost 1 operation, meaning it's O(n).

;; There isn't even a cost to enumerating to the memoized element in the
;; sequence since the enumeration happens back-to-back with the computation of
;; each next element.

;; Thus, memoization would improve the time complexity of the original
;; approach, but would it improve the space complexity? It would not affect
;; either the time or space complexities of Louis's approach, since the
;; original reference is never passed in the recursions.

;; Edit: Actually, Louis's version is exponential in time? Don't know about
;; space complexity. Should run a test and check the actual runtime!

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

(define (sqrt-stream-l x)
  (cons-stream
   1.0
   (stream-map (lambda (guess)
                 (sqrt-improve guess x))
               (sqrt-stream-l x))))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)


(define rt (runtime))
(stream-ref (sqrt-stream-l 2) 10000)
(display (- (runtime) rt))

(newline)

(set! rt (runtime))
(stream-ref (sqrt-stream 2) 10000)
(display (- (runtime) rt))

;; Running on 1000:
;; Louis: 245K
;; Standard: 980

;; Running on 10000:
;; Louis: 52M
;; Standard: 4257

;; So 10x increase in input:
;; Louis: 210x increase in runtime
;; Standard: 4.34x increase in runtime

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(newline)

(set! rt (runtime))
(fib 10)
(display (- (runtime) rt))

;; Compare to unmemoized fib:
;; fib(10): 156
;; fib(20): 6765 (43x)
;; fib(40): 426K (63x)
;; fib(100): Runs too long

;; Based on this, I'd say I was right; Louis's approach has a runtime
;; that growths polynomially with respect to the stream length.
