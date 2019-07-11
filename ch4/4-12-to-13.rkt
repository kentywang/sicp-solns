;;; 4.12

;; Returns pair of list with variable at front and list with value at
;; front, or false
(define (find-binding var env)
  (define (scan vars vals)
    (cond ((null? vars)
           (find-binding var (enclosing-environment env)))
          ((eq? var (car vars))
           (cons vars vals))
          (else (scan (cdr vars)
                      (cdr vals)))))
  (if (eq? env the-empty-environment)
      false
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))

(define (lookup-variable-value var env)
  (let ((binding (find-binding var env)))
    (if binding
        (cadr binding)
        (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (let ((binding (find-binding var env)))
    (if binding
        (set-car! (cdr binding) val)
        (error "Unbound variable: SET!" var))))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (binding (find-binding var
                                ;; Only pass 1st frame.
                                (list frame))))
    (if binding
        (set-car! (cdr binding) val)
        (assign-binding-to-frame! var val frame))))

;;; 4.13

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
        ((unbound? exp)                    ; New
         (eval-unbound exp env))
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
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values
                 (operands exp)
                 env)))
        (else
         (error "Unknown expression
                 type: EVAL" exp))))

(define (make-unbound? exp)
  (tagged-list? exp 'make-unbound!))

;; Unbinds first binding found within full environment. This is my approach
;; because frames are a pretty abstract concept that users might not care
;; to understand. They just want their variable unbound.
(define (eval-unbound exp env)
  ;; Borrowing a proc from 4.12.
  (let (binding (find-binding (unbound-var exp) env))
    (if binding
        (begin (shift! (car binding))
               (shift! (cdr binding))
               'ok)
        (error
         "No variable found in environment to unbind"
         (unbound-var exp)))))

(define (unbound-var exp) (cdr exp))

;; Helper
(define (shift! items)
  "Removes car of list by shifting things up."
  (cond ((null? items)
         (error "Nothing to shift: SHIFT!"))
        ;; Missing handling for if item is just 1 element!
        ((null? (cddr items))
         (set-car! items (cdr items))
         (set-cdr! items '()))
        (else
         (set-car! items (cdr items))
         (shift! (cdr items)))))
