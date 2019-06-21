#lang sicp

;;; Try 1:

(define zero
  (lambda (x) x))

(define one
  (lambda (x) (f x)))

;; Problem: If we are to represent integers as repeated applications of some function, then that function must be passed into the integer representation.

;;; Try 2:

(define zero
  (lambda (f x) x))

(define one
  (lambda (f x) (f x)))

;; We don't want to make higher-value integers into functions that are higher-order than smaller numbers, because that makes it hard to use the same operations on different numbers. We just want bigger numbers to have more fs applied to them.

;; Thus, to apply an operation on a number, we must unwrap it, modify the number, and wrap it back up.

(define (add-1 n)
  (lambda (f x)
    (f (n f x))))

;; Think this works. Now let’s try the addition operation.

(define (+ a b)
  (lambda (f x)
    ((a f x) (b f x))))

;; This probably wouldn't work because we unwrap a to the base level, where it's no longer a procedure that can be applied.

(define (+ a b)
  (lambda (f x)
    (a f (b f x))))

;; This seems to work, too. So it looks like we can rewrite the definitions to return a lambda that takes a function and a value instead of a function that returns a lambda that returns a value. At least for the current operations, it seems we don't need currying.

;; Rewriting for the currying though:

(define (+ a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;; Edit: There's a neat rewrite online that didn't even need the x values in the expressions (including zeroes?):
http://community.schemewiki.org/?sicp-ex-2.6
