#lang sicp
;; (#%require "base.rkt")

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
        ((and? exp)                       ; AND
         (eval-and exp env))
        ((or? exp)                        ; OR
         (eval (or->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values
                 (operands exp)
                 env)))
        (else
         (error "Unknown expression
                 type: EVAL" exp))))

;;; I implement and as a special form.

(define (and? exp) (tagged-list? exp 'and))
(define (and-first exp) (cadr exp))
(define (and-rest exp) (cddr exp))

(define (eval-and exp env)
  (if (null? exp)
      'true
      ;; Relying on implementation language to eval the and LTR.
      (and (true? (eval (and-first exp) env))
           (eval-and (and-rest exp) env))))

;;; I implement or as a derived expression.

(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))

(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (make-if (car clauses)
               'true
               (or->if (cdr clauses)))))
