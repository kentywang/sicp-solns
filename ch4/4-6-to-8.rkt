#lang sicp

(define (eval exp env)
  (cond ((self-evaluating? exp)
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((quoted? exp)
         (text-of-quotation exp))
        ((assignment? exp)
         (eval-assignment exp env))
        ((definition? exp)
         (eval-definition exp env))
        ((if? exp)
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence
          (begin-actions exp)
          env))
        ((cond? exp)
         (eval (cond->if exp) env))
        ((let? exp)                          ; 4.6
         (eval (let->combination exp) env))
        ((let*? exp)                         ; 4.7
         (eval (let*->nested-lets exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values
                 (operands exp)
                 env)))
        (else
         (error "Unknown expression
                 type: EVAL" exp))))

;;; 4.6

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-vars exp) (map car (cadr exp)))
(define (let-exps exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-vars exp)
                     (let-body exp))
        (let-exps exp)))

;;; 4.7

;; let* is just shorthand for nested lets. It's sufficient to convert it to
;; a nested let expression and just eval that.

(define (let*? exp)
  (tagged-list? exp 'let*))

;; Same as let's
(define (let-vars exp) (map car (cadr exp)))
(define (let-exps exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))

(define (make-let vars exps body)
  (cons 'let
        (cons (map list vars exps)
              body)))

(define (let*->nested-lets exp)
  (define (recur vars exps)
    (if (null? vars)
        (let-body exp)
        (make-let (car var)
                  (car exps)
                  (recur (cdr var) (cdr exps)))))
  (recur (let-vars exp) (let-exps exp)))

;; Edit: Could use fold.

;;; 4.8

;; (let f ((a 0) (b 1)) (+ 1 (f a b))) is just:

;; (let ((f (lambda (a b)
;;            (+ 1 (f a b)))))
;;   (f 0 1))

;; Which is just:

;; ((lambda (f)
;;    (lambda (a b)
;;      (+1 (f a b))))
;;  0
;;  1)

;; Initially I thought we needed it to be define, but since the var binding
;; only exists within the let body, it's not.

;;; More robust selectors

(define (let-vars exp)
  (map car
       ((if (symbol? (cadr exp))
            caddr ; Named let, so 3rd element is the bindings.
            cadr) ; Normal let, so 2nd element is the bindings.
        exp)))
(define (let-exps exp)
  (map cadr
       ((if (symbol? (cadr exp))
            caddr
            cadr)
        exp)))
(define (let-body exp)
  ((if (symbol? (cadr exp))
       cdddr ; Named let, so 4th element on are the body.
       cddr) ; Normal let, so 3rd element on are the body.
   exp))

;; New
(define (let-name exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      false)) ; For implementation language to use, so not quoted.

(define (let->combination exp)
  (let ((name (let-name exp))
        (vars (let-vars exp))
        (exps (let-exps exp))
        (body (let-body exp)))
    (if name
        (make-let (list name)
                  (make-lambda vars body)
                  (cons name exps))
        (cons (make-lambda vars body)
              exps)))
