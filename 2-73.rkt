#lang racket

;;; 2.73

;;; 1.
;; The procedure uses data-directed programming to handle the multitude of
;; forms the expression may have. number? and variable? could not be handled
;; by the data-directed form since the general pattern is to pass the first
;; element of the expression to determine second parameter for get, but number?
;; and variable? procedures require the full expression itself.

;;; 2. & 3.
;;; Given

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;;; Deps, with table from http://community.schemewiki.org/?sicp-ex-2.73

(define *the-table* (make-hash));make THE table 
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;;; Main
;; No need for tagging, since the +/* operator is already the tag.

;; Since deriv is now called with the operator stripped out, we need to adjust
;; the selectors for addend, augend, multiplier, mutiplicand, etc.

;; Nothing else needs to be changed.

(define (install-derivs)
  ;; internal procedures
  (define addend car)
  (define augend cadr)
  (define multiplier car)
  (define multiplicand cadr)
  (define base car)
  (define exponent cadr)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) 
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) 
               (=number? m2 0)) 
           0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) 
           (* m1 m2))
          (else (list '* m1 m2))))
  (define (make-exponentiation b e)
    (cond ((=number? e 1) b)
          ((=number? e 0) 1)
          ((and (number? b) (number? e))
           (expt b e))
          (else (list '** b e))))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (deriv-product exp var)
    (make-sum
     (make-product 
      (multiplier exp)
      (deriv (multiplicand exp) var))
     (make-product 
      (deriv (multiplier exp) var)
      (multiplicand exp))))
  (define (deriv-expt exp var)
    (let ((expo-expr (exponent exp))
          (base-expr (base exp)))
      (make-product
       (make-product expo-expr
                     (make-exponentiation
                      base-expr
                      (make-sum expo-expr -1)))
       (deriv base-expr var))))
  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-expt)
  'done)

(install-derivs)

;;; Tests

(deriv '(* (* x y) (+ x 3)) 'x) ; xy * (x+3)
(deriv '(* (** x 2) (** 3 2)) 'x) ; x^2 * 3^2

;;;  4.
;; All you need to do is to swap the order of the 3rd and 4th args in the puts.