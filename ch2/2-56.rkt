#lang sicp

;;; Deps

(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define addend cadr)
(define augend caddr)

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define multiplier cadr)
(define multiplicand caddr)

(define (=number? exp num)
  (and (number? exp) (= exp num)))

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

;;; Main

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ;; New condition
        ((exponentiation? exp)
         (let ((expo-expr (exponent exp))
               (base-expr (base exp)))
           (make-product
            (make-product expo-expr
                          (make-exponentiation
                           base-expr
                           (make-sum expo-expr -1)))
            (deriv base-expr var))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define exponent caddr)
(define base cadr)

(define (make-exponentiation b e)
  (cond ((=number? e 1) b)
        ((=number? e 0) 1)
        ((and (number? b) (number? e))
         (expt b e))
        (else (list '** b e))))

;;; Tests

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(deriv '(** x 3) 'x)
(deriv '(** x 1) 'x)
(deriv '(** x 0) 'x)

;; Works on numeric bases?
(deriv '(* (** x 2) (** 3 2)) 'x) ; x^2 * 3^2

;; Returns (* (* 2 x) (** 3 2)), which isn't the simplest because (** 3 2) was
;; not considered a number in make-product, but this happens with an expression
;; like (deriv '(* x (+ 3 2)) 'x) too, where the returned value is (+ 3 2).

;; Works on variable exponents?
(deriv '(** 3 x) 'x)

;; Works on variable bases and exponents?
(deriv '(** x x) 'x)