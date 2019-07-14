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
        (add-binding-to-frame! var val frame))))

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

;; Initial approach was to reuse the find-binding procedure defined for
;; 4.12. But it's not the best here because we need to reference more than
;; the tail of a list in order to delete (or rather, derefernce a list
;; completely).

(define (eval-unbound exp env)
  (define (env-loop var env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop
              var
              (enclosing-environment env)))
            ((eq? var (car vars))
             (if (= 1 (length (frame-variables (first-frame env))))
                 ;; Only one element, so deref list completely.
                 (set-car! env (make-frame '() '()))
                 (begin (shift! vars)    ; Would be more performant to keep
                        (shift! vals)))) ; ref of preceding element to edit
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Nonexistent variable: MAKE-UNBOUND!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop (unbound-var exp) env))

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
