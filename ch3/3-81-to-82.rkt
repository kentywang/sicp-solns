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

;; (define (make-random-numbers n)
;;   (define recur
;;     (stream-cons n (stream-map rand-update
;;                                recur)))
;;   recur)

;;; Main

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
