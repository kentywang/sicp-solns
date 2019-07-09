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
        ((let*? exp)                         ; New
         (eval (let*->nested-lets exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values
                 (operands exp)
                 env)))
        (else
         (error "Unknown expression
                 type: EVAL" exp))))

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
