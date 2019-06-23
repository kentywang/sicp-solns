#lang sicp

;;; Deps

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;;; Main

;; Strategy: keep a fast and a slow pointer, where the fast pointer should
;; overlap the slow pointer exactly when the slow pointer loops back to the
;; start.

(define (cycles? l)
  (let ((slow '())
        (fast '()))
    (define (iter)
      (cond ((or (null? fast)
                 (null? (cdr fast))) false)
            ((eq? slow fast) true)
            (else (set! slow (cdr slow))
                  (set! fast (cddr fast))
                  (iter))))
    (if (or (null? l)
            (null? (cdr l)))
        false
        (begin (set! slow (cdr l))
               (set! fast (cddr l))
               (iter)))))

;; Edit: I could've written this without keeping state, and instead passed
;; fast and slow via arguments to iter. I think my method is still constant
;; space (and linear time) though, since I'm just defining pointers to
;; existing list structures.

;;; Tests

(define x '(a b c d e f g))
(make-cycle (cddr x))

(define y '(a b c d e f g))

(cycles? x)
(cycles? y)
