#lang sicp

;; Constructor, the procedure with local state
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (dispatch m)
      "Dispatches the internal stuctures (selectors) and procedures (mutators)."
      (m front-ptr rear-ptr set-front-ptr! set-rear-ptr!))
    dispatch))

;; Selectors
(define (empty-queue? queue)
  (queue (lambda (front rear set-front! set-rear!)
           (null? front))))

(define (front-queue queue)
  (queue (lambda (front rear set-front! set-rear!)
           (if (empty-queue? queue)
               (error "FRONT called with an
              empty queue" queue)
               (car front)))))

;; Mutators
(define (insert-queue! queue item)
  (queue (lambda (front rear set-front! set-rear!)
           (let ((new-pair (list item)))
             (if (empty-queue? queue)
                 (begin (set-front! new-pair)
                        (set-rear! new-pair)
                        queue)
                 (begin (set-cdr! rear new-pair) ; (set! (cdr rear) new-pair) wouldn't work.
                        (set-rear! new-pair)
                        queue))))))

(define (delete-queue! queue)
  (queue (lambda (front rear set-front! set-rear!)
           (if (empty-queue? queue)
               (error "DELETE! called with an empty queue" queue)
               (begin (set-front! (cdr front))
                      queue)))))

;;; Tests

;; From 3.21:
(define (print-queue queue)
  (display 'Queue:)
  (display (queue (lambda (f r sf! sr!) f)))
  (newline))

(define q1 (make-queue))

(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
