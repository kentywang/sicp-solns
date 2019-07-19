#lang sicp

(define apply-in-underlying-scheme apply)

;;; Eval

(define (eval exp env)
;  (display "eval: ")
;  (display exp)
;  (newline)
  (cond ((self-evaluating? exp)
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((quoted? exp)
         (eval-quotation (text-of-quotation exp) env))           ; 4.33
        ((assignment? exp)
         (eval-assignment exp env))
        ((definition? exp)
         (eval-definition exp env))
        ((make-unbound? exp)                 ; MAKE-UNBOUND!
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
        ((and? exp)                          ; AND
         (eval-and exp env))
        ((or? exp)                           ; OR
         (eval (or->if exp) env))
        ((let? exp)                          ; LET
         (eval (let->combination exp) env))
        ((let*? exp)                         ; LET*
         (eval (let*->nested-lets exp) env))
        ((letrec? exp)                       ; LETREC
         (eval (scan-out-defines
                (add-defs-to-body
                 (let-vars exp)
                 (let-exps exp)
                 (let-body exp)))
               env))
        ((while? exp)                        ; WHILE
         (eval-while exp env))
        ((application? exp)
         (apply-in-interpreted-scheme (actual-value (operator exp) env)
                                      (operands exp)
                                      env))
        (else
         (error "Unknown expression
                 type: EVAL" exp))))

;;; Apply

(define (apply-in-interpreted-scheme procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values 
           arguments 
           env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args 
            arguments 
            env)   ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure 
                      type: APPLY" 
                     procedure))))

;; Actual value
(define (actual-value exp env)
  (force-it (eval exp env)))

;;; Delaying

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;;; Memoization

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) 
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result
                (actual-value 
                 (thunk-exp obj)
                 (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           ;; replace exp with its value:
           (set-car! (cdr obj) result) 
           ;; forget unneeded env:
           (set-cdr! (cdr obj) '()) 
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;;; Procedure arguments

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value 
             (first-operand exps) 
             env)
            (list-of-arg-values 
             (rest-operands exps)
             env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it 
             (first-operand exps) 
             env)
            (list-of-delayed-args 
             (rest-operands exps)
             env))))

;;; Conditionals

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) 
                           env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;; Sequences

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps)
                        env))))

;;; Assignments and definitions

(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;;; AND

(define (and? exp) (tagged-list? exp 'and))

(define (eval-and exp env)
  (define (and-clauses exp) (cdr exp))
  (define (and-first exps) (car exps))
  (define (and-rest exps) (cdr exps))
  (define (recur exps)
    (if (null? exps)
        'true
        ;; Relying on implementation language to eval the and LTR.
        (and (true? (eval (and-first exps) env))
             (recur (and-rest exps)))))
  (recur (and-clauses exp)))

;;; OR

(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))

(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (make-if (car clauses)
               'true
               (expand-or-clauses (cdr clauses)))))

;;; LET

(define (let? exp)
  (tagged-list? exp 'let))

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

(define (let-name exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      false)) ; For implementation language to use, so not quoted.

(define (let->combination exp)
  (let* ((name (let-name exp))
         (vars (let-vars exp))
         (exps (let-exps exp))
         (body (let-body exp))
         (let-bindings (make-lambda vars body)))
    (if name
        (list
         (make-lambda ; Lambda expression scopes the function binding.
          '()
          (list (list 'define name let-bindings)
                (cons name exps))))
        (if (null? vars)
            (sequence->exp body)
            (cons let-bindings exps)))))

;;; LET*

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (make-let vars exps body)
  (cons 'let
        (cons (map list vars exps)
              body)))

(define (let*->nested-lets exp)
  (define (recur vars exps)
    (if (null? vars)
        (sequence->exp (let-body exp))
        (make-let (list (car vars))
                  (list (car exps))
                  (list (recur (cdr vars) (cdr exps))))))
  (recur (let-vars exp) (let-exps exp)))

;;; LETREC

(define (letrec? exp)
  (tagged-list? exp 'letrec))

(define (add-defs-to-body vars exps body)
  (if (null? vars)
      body
      (cons (list 'define (car vars) (car exps))
            (add-defs-to-body (cdr vars) (cdr exps) body))))

;;; WHILE

(define (while? exp)
  (tagged-list? exp 'while))

(define (eval-while exp env)
  (define while-predicate (lambda (exp) (cadr exp)))
  (define while-body (lambda (exp) (cddr exp)))
  (cond ((true? (eval (while-predicate exp) env)) ; Alternatively, could
         (eval-sequence (while-body exp) env)     ; use while.
         (eval-while exp env))))

;;; MAKE-UNBOUND!

(define (make-unbound? exp)
  (tagged-list? exp 'make-unbound!))

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

(define (unbound-var exp) (cadr exp))

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

;;; Representing expressions

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

;; 4.33
(define (eval-quotation exp env)
  (cond ((pair? exp) (eval (make-cons exp) env))
        (else exp)))

;; 4.33
(define (make-cons exp)
  (if (null? exp)
      nil
      (list 'cons (car exp)
            (make-cons (cdr exp)))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate
                 consequent
                 alternative)
  (list 'if
        predicate
        consequent
        alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))

;;; New selectors
(define (cond-special-clause? clause)
  (eq? (cadr clause) '=>))
(define (cond-special-proc clause)
  (caddr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                 (cond-actions first))
                (error "ELSE clause isn't
                        last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     ;; For (⟨test⟩ => ⟨recipient⟩)
                     (if (cond-special-clause? first)
                         (list (cond-special-proc first)
                               (cond-predicate first))
                         (sequence->exp
                          (cond-actions first)))
                     (expand-clauses rest))))))

;;; Testing of predicates

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;;; Representing procedures

(define (make-procedure parameters body env)
  ;; (list (scan-out-defines body)) because new body is supposed to be a
  ;; sequence of expressions.
  ;; Could really use an abstraction layer to deal with this.
  (list 'procedure parameters (list (scan-out-defines body)) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;;; Operations on environments

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied"
                 vars
                 vals)
          (error "Too few arguments supplied"
                 vars
                 vals))))

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
        (if (eq? '*unassigned*
                 (cadr binding))
            (error "Unassigned variable" var)
            (cadr binding))
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

(define (scan-out-defines body)
  (define (iter body defs rest)
    (if (null? body)
        (rebuild defs (reverse rest))
        (if (definition? (car body))
            (iter (cdr body)
                  (cons (car body) defs)
                  rest)
            (iter (cdr body)
                  defs
                  (cons (car body) rest)))))
  (define (rebuild defs rest)
    (make-let (map definition-variable defs)
              (map (lambda (_) ''*unassigned*) defs)
              (append (make-sets defs) rest)))
  (define (make-sets defs)
    (if (null? defs)
        '()
        (cons (list 'set!
                    (definition-variable (car defs))
                    (definition-value (car defs)))
              (make-sets (cdr defs)))))
  (iter body '() '()))

;;; Binding the primitives

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'eq? eq?) ; 4.33
        (list 'list-underlying list) ; 4.34
        (list 'list-ref-underlying list-ref) ; 4.34
        (list '= =))) ; All other primitives go here.

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment
  (setup-environment))

;;; REPL

;; 4.33
;(eval-sequence
; '((define (cons x y) (lambda (m) (m x y)))
;   (define (car z) (z (lambda (p q) p)))
;   (define (cdr z) (z (lambda (p q) q))))
; the-global-environment)

;; 4.34
(eval-sequence
 '((define (cons x y) (list-underlying 'cons-return (lambda (m) (m x y))))
   (define (car z) ((list-ref-underlying z 1) (lambda (p q) p)))
   (define (cdr z) ((list-ref-underlying z 1) (lambda (p q) q))))
 the-global-environment)

(define input-prompt  ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

;; 4.34
(define (cons-return? z)
  (and (pair? z)
       (tagged-list? z 'cons-return)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value 
                   input 
                   the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline)
  (display string) (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

;; 4.34 modified
(define (user-print object)
  (display (cond ((compound-procedure? object)
                  (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>))
                 ((cons-return? object)
                  (let ((cons-env (cadddr (cadr object)))
                        ;; Should be ('thunk 'x <cons-env>)
                        (a (eval (list 'car (list 'quote object))
                                 the-global-environment))
                        ;; Should be ('thunk 'y <cons-env>)
                        (b (eval (list 'cdr (list 'quote object))
                                 the-global-environment)))
                    ;; Always thunks if cons is lazy.
                    ;; There's gotta be a more straightforward way to do this.
                    (list (thunk-exp (lookup-variable-value (thunk-exp a) cons-env))
                          '&
                          ;; Has issues with for instance (cdr integers)
                          (thunk-exp (lookup-variable-value (thunk-exp b) cons-env)))))
                 (else object))))

(define (display-runtime time)
  (display "Runtime: ")
  (newline)
  (display time)
  (newline))

(driver-loop)
