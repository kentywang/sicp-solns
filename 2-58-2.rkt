#lang sicp

;;; Deps

(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define multiplier car)

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

;;; Changes already modified in part 1 for infix notation.

(define (sum? x)
  (and (pair? x) (contains? '+ x)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list m1 '* m2))))

;; Strategy:
;; - Since * higher precedence than +, split expressions by +, and the
;;   subexpressions will evaluate first.
;; - + splits can be grouped into lists, which as demonstrated in an earlier
;;   problem, will have deriv applied to them recursively, thus addressing the
;;   variable arity problem. * splits don't need to account for non-*
;;   operators, so it's simpler; we only need one side of the * split to be a
;;   list.

(define (contains? x items)
  "Check if list contains an element."
  (cond ((null? items) false)
        ((eq? (car items) x) true)
        (else (contains? x (cdr items)))))

(define (list-before item)
  "Get list before first occurence of element."
  (lambda (things)
    (if (or (null? things) 
            (eq? (car things) item))
        nil
        (cons (car things) ((list-before item) (cdr things))))))

(define (list-after item)
  "Get list after first occurence of element."
  (define (recur things)
    (cond ((null? things) nil)
          ((eq? (car things) item) (cdr things))
          (else (recur (cdr things)))))
  (lambda (items)
    (recur items)))

(define (addend e)
  (let ((lb ((list-before '+) e)))
    (if (= (length lb) 1)
        (car lb)
        lb)))

(define (augend e)
  (let ((la ((list-after '+) e)))
    (if (= (length la) 1)
        (car la)
        la)))

(define (multiplicand e)
  (let ((la ((list-after '*) e)))
    (if (= (length la) 1)
        (car la)
        la)))

;;; Tests

(deriv '(x + (3 * (x + (y + 2)))) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x + x * y * x + 2) 'x)