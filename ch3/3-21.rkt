#lang sicp

;; Queue prints the values of the car and the cdr of the queue, which are
;; the entirely of the queue and the last item of the queue. The empty
;; queue that shows b as the last item is because the rear pointer is still
;; pointing to b, which is a nonissue in the actual utilization of the
;; queue.

;;; Deps

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

;; Constructor
(define (make-queue) (cons '() '()))

;; Selectors
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an
              empty queue" queue)
      (car (front-ptr queue))))

;; Mutators
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue)
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with
                 an empty queue" queue))
        (else (set-front-ptr!
              queue
               (cdr (front-ptr queue)))
              queue)))

;;; Main

(define (print-queue queue)
  (display 'Queue:)
  (display (front-ptr queue))
  (newline))

;;; Tests

(define q1 (make-queue))

(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
