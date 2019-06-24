#lang sicp

;; Strategy: create node compound data structure that adds another pointer
;; to the previous element. This allows for doubly linked list that allows
;; us to keep operations to constant time. This allows the actual deque
;; code to remain mostly the same as the queue.

;; We're updating the front-ptr and rear-ptr state with each mutation,
;; but we don't update the node pointers, prev and next, on deletions.
;; This doesn't seem to cause any issues (except needing more care in
;; the print function).

;;; Node: internal data structure used by deque.

;; Constructor
(define (node a b c)
  (cons a (cons b c)))

;; Selectors
(define (value n) (car n))
(define (prev n) (cadr n))
(define (next n) (cddr n))

;; Mutators
(define (set-prev! n x) (if (pair? n)
                            (set-car! (cdr n) x)))
(define (set-next! n x) (if (pair? n)
                            (set-cdr! (cdr n) x)))

;;; Deque

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item)
  (set-car! deque item))
(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

;; Constructor
(define (make-deque) (cons '() '()))

;;; Selectors

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an
              empty deque" deque)
      (value (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an
              empty deque" deque)
      (value (rear-ptr deque))))

;;; Mutators

(define (front-insert-deque! deque item)
  (let ((n (node item '() (front-ptr deque))))
    (if (empty-deque? deque)
        (set-rear-ptr! deque n)
        (set-prev! (front-ptr deque) n))
    (set-front-ptr! deque n)
    deque))

(define (rear-insert-deque! deque item)
  (let ((n (node item (rear-ptr deque) '())))
    (if (empty-deque? deque)
        (set-front-ptr! deque n)
        (set-next! (rear-ptr deque) n))
    (set-rear-ptr! deque n)
    deque))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with
                 an empty deque" deque))
        ;; Only one item left, so clear both ptrs. Need this for keeping
        ;; both pointer states accurate.
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         deque)
        (else (set-front-ptr! deque (next (front-ptr deque)))
              deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with
                 an empty deque" deque))
        ;; Only one item left, so clear both ptrs. Need this for keeping
        ;; both pointer states accurate.
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         deque)
        (else (set-rear-ptr! deque (prev (rear-ptr deque)))
              deque)))

;; Helper
(define (print deque)
  (define (recur d)
    (if (not (null? d))
        (begin (display (value d))
               (display " ")
               (if (not (eq? d (rear-ptr deque)))
                   (recur (next d))))))
  (display "Deque: ")
  (recur (front-ptr deque))
  (newline))

;;; Tests

(define x (make-deque))
(front-insert-deque! x 'a)
(rear-delete-deque! x)
(rear-insert-deque! x 'b)
(front-delete-deque! x)
(print x)

(front-insert-deque! x 5)
(rear-insert-deque! x 6)
(front-insert-deque! x 4)
(rear-insert-deque! x 7)
(front-insert-deque! x 3)
(rear-insert-deque! x 8)
(print x)

(rear-delete-deque! x)
(rear-delete-deque! x)
(front-delete-deque! x)
(front-delete-deque! x)
(print x)
