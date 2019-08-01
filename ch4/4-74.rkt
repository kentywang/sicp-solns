#lang racket

;;; 4.74

;;; 1.

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (x)
                               (not (stream-null? x)))
                             stream)))

;;; 2.

;; Don't think so. The question is basically if the original interleaving
;; procedure would scramble the stream if it were comprised of empty streams
;; and singleton streams. The interleaving of interleave and flatten mean that
;; we'd enumerate sequentially if we only had singletons/empties.