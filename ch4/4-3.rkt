;; 4.3
;; This approach means losing the decoupling of syntax with usage. And
;; needs type tags for self-evaluating expressions, variables, and
;; procedure applications.

(put 'eval 'self-evaluating
     (lambda (exp env)
       exp))
(put 'eval 'variable lookup-variable-value)
(put 'eval 'quote
     (lambda (exp env)
       (text-of-quotation exp)))
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda
     (lambda (exp env)
       (make-procedure
        (lambda-parameters exp)
        (lambda-body exp)
        env)))
(put 'eval 'begin
     (lambda (exp env)
       (eval-sequence
        (begin-actions exp)
        env)))
(put 'eval 'cond
     (lambda (exp env)
       (eval (cond->if exp) env)))
(put 'eval 'call
     (lambda (exp env)
       (apply (eval (operator exp) env)
              (list-of-values
               (operands exp)
               env))))

(define (eval exp env)
  ((apply-generic 'eval (car exp)) exp env))

;; Edit: Could just include exceptions for variable? and self-evaluating?
;; clauses, like 2.73. That way, we don't need to add tags.
