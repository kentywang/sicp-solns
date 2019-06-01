#lang sicp

;;; Deps

(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

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
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define addend cadr)

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define multiplier cadr)

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

;; Strategy: modify how to retrieve the second operand of a sum/product to be
;; recursive. 

(define (process x f)
  (define (recur t)
    (let ((first (car t))
          (rest (cdr t)))
      (if (null? rest)
          first
          (f first (recur rest)))))
  (recur (cddr x))) ; The list after first operand.

(define (augend s) (process s make-sum))
(define (multiplicand p) (process p make-product))

;; Tests

(deriv '(+ x 3) 'x)
(deriv '(+ x 3 x) 'x)
(deriv '(+ x 3 (* x 2)) 'x)
(deriv '(+ x x x x) 'x)
(deriv '(* x y (+ x 3)) 'x)

;; Edit: I don't actually need to do recursion and call make-sum from augend or
;; mutiplicand to get a fully expanded expression, since deriv will be applied
;; to the subexpressions of a sum or product.

;; A simple (cons '+ (all-but-first-term s)))) is all augend needs to return in
;; the alternative clause of the if expression.

;; http://wiki.drewhess.com/wiki/SICP_exercise_2.57