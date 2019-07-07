#lang racket

;;; Deps

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
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

(define random-init (random 100))

(define (rand-update x) (+ 1 x)) ; Super random.

(define random-numbers
  (stream-cons random-init
               (stream-map rand-update
                           random-numbers)))

(define (monte-carlo experiment-stream
                     passed
                     failed)
  (define (next passed failed)
    (stream-cons
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-rest experiment-stream)
      passed
      failed)))
  (if (stream-first experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (area a b m n)
  (* (- b a) (- n m))) ; Assumes b > a, n > m.

;;; 3.81

(define (rand req-stream)
  (define recur
    (stream-cons
     random-init
     (stream-map (lambda (e f g)
                   (cond ((eq? e 'generate)
                          (rand-update g))
                         ((eq? e 'reset)
                          f)
                         (else ; If request is a number
                          (rand-update g))))
                 req-stream
                 (stream-rest req-stream)
                 recur)))
    (stream-rest recur))

;;; Tests

(define reqs (stream* 'generate
                      'generate
                      'generate
                      'reset
                      5
                      'generate
                      'generate
                      'generate
                      reqs))

(print 20 (rand reqs))

;;; 3.82

(define (estimate-integral p x1 x2 y1 y2)
  (define (test-stream)
    (stream-cons
     (p (random-in-range x1 x2)
        (random-in-range y1 y2))
     (test-stream)))
  (define test-area (area x1 x2 y1 y2))
  (stream-map
   (lambda (pass-ratio) (* (exact->inexact pass-ratio)
                           test-area))
   (monte-carlo (test-stream) 0 0)))

;;; Tests

(define unit-circle-a ; Also is pi.
  (estimate-integral
   (lambda (x y)
     (>= 1 (+ (expt x 2) (expt y 2))))
   -1
   1
   -1
   1))

(print 100 unit-circle-a)

;; Edit: Could be made more streamy. For instance, we can have a stream of
;; random pairs to stream-map over with p. Also, could have just scaled the
;; monte-carlo stream by the area.
