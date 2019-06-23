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

(define (cycles? l)
  (let ((seen '()))
    (define (recur x)
      (cond ((not (pair? x)) false)
            ((memq x seen) true)
            (else (set! seen (cons x seen))
                  (recur (cdr x)))))
    (recur l)))

;;; Tests

(define x '(a b c d e f g))
(make-cycle x)

(define t1 (cons 'a 'b))
(define t2 (cons t1 t1))

(cycles? x)
(cycles? t2)
