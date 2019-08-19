#lang sicp

;; 5.30.1 Strategy: We have lookup procedures return an tagged list
;; of type "error-code", which then gets checked as early as possible
;; in the machine code, so we can immediately go to signal-error.
;; Only implemented for variable lookup.

;; 5.30.2: similar strategy. Only implemented pre-cheking of valid variables
;; for / and car.

(define (make-machine register-names 
                      ops 
                      controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) 
                 register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) 
               (set! contents value)))
            (else
             (error "Unknown request: 
                     REGISTER"
                    message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth 
            (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth
                  (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)

    (define (print-statistics)
      (newline)
      (display (list 'total-pushes 
                     '= 
                     number-pushes
                     'maximum-depth
                     '=
                     max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize)
             (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request: STACK"
                    message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ;; We _could_ make these registers, too.
        (inst-ct 0)
        (trace? false)
        ;; 5.17: And now we shall.
        (curr-inst-label (make-register 'curr-inst-label)))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () 
                         (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () 
                         (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) 
                 (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error 
             "Multiply defined register: " 
             name)
            (set! register-table
                  (cons 
                   (list name 
                         (make-register name))
                   register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val 
               (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" 
                     name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (let* ((the-inst (car insts))
                     (the-inst-text (instruction-text the-inst)))
                ;; 5.16
                (if trace?
                    (begin (display (instruction-label the-inst))
                           (display ':)
                           (display the-inst-text)
                           (newline)))
                ((instruction-execution-proc 
                  the-inst))
                (set! inst-ct (inc inst-ct)) ; 5.15
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! 
                pc
                the-instruction-sequence)
               (execute))
              ((eq? 
                message 
                'install-instruction-sequence)
               (lambda (seq) 
                 (set! 
                  the-instruction-sequence 
                  seq)))
              ((eq? message 
                    'allocate-register) 
               allocate-register)
              ((eq? message 'get-register) 
               lookup-register)
              ((eq? message 
                    'install-operations)
               (lambda (ops) 
                 (set! the-ops 
                       (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) 
               the-ops)
              ((eq? message 'print-inst-ct) 
               inst-ct)
              ((eq? message 'reset-inst-ct)
               (set! inst-ct 0))
              ((eq? message 'trace-on) ; 5.15
               (set! trace? true))
              ((eq? message 'trace-off)
               (set! trace? false))
              (else (error "Unknown request: 
                            MACHINE"
                           message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents 
         machine register-name)
  (get-contents 
   (get-register machine register-name)))

(define (set-register-contents! 
         machine register-name value)
  (set-contents! 
   (get-register machine register-name) 
   value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;; 5.17
(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)
    'NO-LABEL))

;; 5.17
(define (extract-labels text receive curr-label)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive 
                   insts
                   (cons 
                    (make-label-entry 
                     next-inst
                     insts)
                    labels))
               (receive 
                   (cons (make-instruction 
                          next-inst
                          curr-label)
                         insts)
                   labels))))
       (if (symbol? (car text))
           (car text)
           curr-label))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) 
         labels
         machine
         pc
         flag
         stack
         ops)))
     insts)))

;; 5.17: Modified to include label for instruction
(define (make-instruction text curr-label)
  (list text '() curr-label))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst)
  (cadr inst))
(define (instruction-label inst) (caddr inst))
(define (set-instruction-execution-proc!
         inst
         proc)
  (set-car! (cdr inst) proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE" 
               label-name))))

(define (make-execution-procedure 
         inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign 
          inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test 
          inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch 
          inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform
          inst machine labels ops pc))
        (else (error "Unknown instruction 
                      type: ASSEMBLE"
                     inst))))

(define (make-assign 
         inst machine labels operations pc)
  (let ((target 
         (get-register 
          machine 
          (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp 
                machine
                labels
                operations)
               (make-primitive-exp
                (car value-exp)
                machine
                labels))))
      (lambda ()   ; execution procedure
                   ; for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define 
  (make-test 
   inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition 
                machine
                labels
                operations)))
          (lambda () 
            (set-contents! 
             flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: 
                ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define 
  (make-branch 
   inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label 
                labels 
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: 
                ASSEMBLE"
               inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label 
                   labels
                   (label-exp-label dest))))
             (lambda () 
               (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register 
                   machine
                   (register-exp-reg dest))))
             (lambda ()
               (set-contents! 
                pc
                (get-contents reg)))))
          (else (error "Bad GOTO instruction: 
                        ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register 
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name 
         stack-instruction)
  (cadr stack-instruction))

(define (make-perform 
         inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action
                machine
                labels
                operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction: 
                ASSEMBLE"
               inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label 
                 labels
                 (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register
                   machine
                   (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: 
                      ASSEMBLE"
                     exp))))

(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (cadr exp))
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (constant-exp-value exp)
  (cadr exp))
(define (label-exp? exp)
  (tagged-list? exp 'label))
(define (label-exp-label exp) 
  (cadr exp))

(define (make-operation-exp
         exp machine labels operations)
  (let ((op (lookup-prim 
             (operation-exp-op exp)
             operations))
        (aprocs
         (map (lambda (e)
                ;; 5.9
                (if (label-exp? e)
                    (error "Using label in operation: ASSEMBLE"
                           exp)
                    (make-primitive-exp 
                     e machine labels)))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p))
                              aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))

;;; Scheme implementations of instructions


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

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
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

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


(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;;**following needed only to implement COND as derived expression,
;;; not needed by eceval machine in text.  But used by compiler

;; from 4.1.2
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (true? x)
  (not (eq? x false)))

;;* not used by eceval itself -- used by compiled code when that
;; is run in the eceval machine
(define (false? x)
  (eq? x false))

;;following compound-procedure operations not used by compiled code
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
;;(end of compound procedures)


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
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (list 'error-code "Unbound variable" var) ; 5.30
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;; 5.30
(define primitive-procedures
  (list (list 'car (lambda (pair)
                     (if (pair? pair)
                         (car pair)
                         (list 'error-code "Can't apply car to a non-pair." pair))))
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	;;above from book -- here are some more
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '= =)
	(list '/ (lambda (a b)
                   (if (= b 0)
                       (list 'error-code "Shan't div by 0." a b)
                       (/ a b))))
	(list '> >)
	(list '< <)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;; Simulation of new machine operations needed by
;;;  eceval machine (not used by compiled code)

;;; From section 5.4.1 footnote
(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))

;;; From section 5.4.2 footnote, for non-tail-recursive sequences
(define (no-more-exps? seq) (null? seq))

;;; From section 5.4.4 footnote
(define (get-global-environment)
  the-global-environment)
;; will do following when ready to run, not when load this file
;;(define the-global-environment (setup-environment))

;;; 5.23

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

;;; 5.24

(define (primitive-cond? exp) (tagged-list? exp 'p-cond))
(define (first-clause clauses) (cadr clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (no-clauses? clauses) (null? clauses))
(define (is-else? pred) (equal? 'else pred))

;;; 5.30

(define (error-code? val) (tagged-list? val 'error-code))

;;; The full scheme machine
(define scheme-machine
  (make-machine
    '(exp env val continue proc argl unev)
    (list   (list 'self-evaluating? self-evaluating?)
            (list 'quoted? quoted?)
            (list 'text-of-quotation text-of-quotation)
            (list 'variable? variable?)
            (list 'assignment? assignment?)
            (list 'assignment-variable assignment-variable)
            (list 'assignment-value assignment-value)
            (list 'definition? definition?)
            (list 'definition-variable definition-variable)
            (list 'definition-value definition-value)
            (list 'lambda? lambda?)
            (list 'lambda-parameters lambda-parameters)
            (list 'lambda-body lambda-body)
            (list 'if? if?)
            ;; 5.23
            (list 'cond? cond?)
            (list 'cond->if cond->if)
            (list 'let? let?)
            (list 'let->combination let->combination)
            ;; 5.24
            (list 'first-clause first-clause)
            (list 'rest-clauses rest-clauses)
            (list 'no-clauses? no-clauses?)
            (list 'is-else? is-else?)
            (list 'cond-clauses cond-clauses) 
            (list 'cond-else-clause? cond-else-clause?)
            (list 'cond-predicate cond-predicate)
            (list 'cond-actions cond-actions)
            (list 'primitive-cond? primitive-cond?)
            
            (list 'if-predicate if-predicate)
            (list 'if-consequent if-consequent)
            (list 'if-alternative if-alternative)
            (list 'begin? begin?)
            (list 'begin-actions begin-actions)
            (list 'last-exp? last-exp?)
            (list 'first-exp first-exp)
            (list 'rest-exps rest-exps)
            (list 'application? application?)
            (list 'operator operator)
            (list 'operands operands)
            (list 'no-operands? no-operands?)
            (list 'first-operand first-operand)
            (list 'rest-operands rest-operands)
            (list 'true? true?)
            (list 'make-procedure make-procedure)
            (list 'compound-procedure? compound-procedure?)
            (list 'procedure-parameters procedure-parameters)
            (list 'procedure-body procedure-body)
            (list 'procedure-environment procedure-environment)
            (list 'extend-environment extend-environment)
            (list 'lookup-variable-value lookup-variable-value)
            (list 'set-variable-value! set-variable-value!)
            (list 'define-variable! define-variable!)
            (list 'primitive-procedure? primitive-procedure?)
            (list 'apply-primitive-procedure apply-primitive-procedure)
            (list 'prompt-for-input prompt-for-input)
            (list 'announce-output announce-output)
            (list 'user-print user-print)
            (list 'empty-arglist empty-arglist)
            (list 'adjoin-arg adjoin-arg)
            (list 'last-operand? last-operand?)
            (list 'no-more-exps? no-more-exps?)	;for non-tail-recursive machine
            (list 'get-global-environment get-global-environment)
            (list 'read read)
            (list 'error-code? error-code?)) ; 5.30
    '(read-eval-print-loop
        (perform (op initialize-stack))
        (perform (op prompt-for-input)
                 (const ";;; EC-Eval input:"))
        (assign exp (op read))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (label eval-dispatch))
      print-result
        (perform (op print-stack-statistics))
        (perform (op announce-output)
                 (const ";;; EC-Eval value:"))
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))
      eval-dispatch
        (test (op self-evaluating?) (reg exp))
        (branch (label ev-self-eval))
        (test (op variable?) (reg exp))
        (branch (label ev-variable))
        (test (op quoted?) (reg exp))
        (branch (label ev-quoted))
        (test (op assignment?) (reg exp))
        (branch (label ev-assignment))
        (test (op definition?) (reg exp))
        (branch (label ev-definition))
        (test (op if?) (reg exp))
        (branch (label ev-if))
        (test (op cond?) (reg exp)) ; 5.23
        (branch (label ev-cond)) ; 5.23
        (test (op let?) (reg exp)) ; 5.23
        (branch (label ev-let)) ; 5.23
        (test (op primitive-cond?) (reg exp)) ; 5.23
        (branch (label ev-p-cond)) ; 5.23
        (test (op lambda?) (reg exp))
        (branch (label ev-lambda))
        (test (op begin?) (reg exp))
        (branch (label ev-begin))
        (test (op application?) (reg exp))
        (branch (label ev-application))
        (goto (label unknown-expression-type))
      ev-self-eval
        (assign val (reg exp))
        (goto (reg continue))
      ev-variable
        (assign val
                (op lookup-variable-value)
                (reg exp)
                (reg env))
        ;; 5.30: If val is error code, print immediately. Let read-eval-print-loop
        ;; entry point handle cleaning up the stack.
        (test (op error-code?) (reg val))
        (branch (label signal-error))
        (goto (reg continue))
      ev-quoted
        (assign val
                (op text-of-quotation)
                (reg exp))
        (goto (reg continue))
      ev-lambda
        (assign unev
                (op lambda-parameters)
                (reg exp))
        (assign exp 
                (op lambda-body)
                (reg exp))
        (assign val 
                (op make-procedure)
                (reg unev)
                (reg exp)
                (reg env))
        (goto (reg continue))
      ev-application
        (save continue)
        (save env)
        (assign unev (op operands) (reg exp))
        (save unev)
        (assign exp (op operator) (reg exp))
        (assign
         continue (label ev-appl-did-operator))
        (goto (label eval-dispatch))
      ev-appl-did-operator
        (restore unev)             ; the operands
        (restore env)
        (assign argl (op empty-arglist))
        (assign proc (reg val))    ; the operator
        (test (op no-operands?) (reg unev))
        (branch (label apply-dispatch))
        (save proc)
      ev-appl-operand-loop
        (save argl)
        (assign exp
                (op first-operand)
                (reg unev))
        (test (op last-operand?) (reg unev))
        (branch (label ev-appl-last-arg))
        (save env)
        (save unev)
        (assign continue 
                (label ev-appl-accumulate-arg))
        (goto (label eval-dispatch))
      ev-appl-accumulate-arg
        (restore unev)
        (restore env)
        (restore argl)
        (assign argl 
                (op adjoin-arg)
                (reg val)
                (reg argl))
        (assign unev
                (op rest-operands)
                (reg unev))
        (goto (label ev-appl-operand-loop))
      ev-appl-last-arg
        (assign continue 
                (label ev-appl-accum-last-arg))
        (goto (label eval-dispatch))
        ev-appl-accum-last-arg
        (restore argl)
        (assign argl 
                (op adjoin-arg)
                (reg val)
                (reg argl))
        (restore proc)
        (goto (label apply-dispatch))
      apply-dispatch
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-apply))
        (test (op compound-procedure?) (reg proc))
        (branch (label compound-apply))
        (goto (label unknown-procedure-type))
      unknown-expression-type
        (assign 
         val
         (const unknown-expression-type-error))
        (goto (label signal-error))
      unknown-procedure-type
        ; clean up stack (from apply-dispatch):
        (restore continue)    
        (assign 
         val
         (const unknown-procedure-type-error))
        (goto (label signal-error))
      signal-error
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))
      primitive-apply
        (assign val (op apply-primitive-procedure)
                (reg proc)
                (reg argl))
        (test (op error-code?) (reg val)) ; 5.30
        (branch (label signal-error))
        (restore continue)
        (goto (reg continue))
      compound-apply
        (assign unev 
                (op procedure-parameters)
                (reg proc))
        (assign env
                (op procedure-environment)
                (reg proc))
        (assign env
                (op extend-environment)
                (reg unev)
                (reg argl)
                (reg env))
        (assign unev
                (op procedure-body)
                (reg proc))
        (goto (label ev-sequence))
      ev-begin
        (assign unev
                (op begin-actions)
                (reg exp))
        (save continue)
        (goto (label ev-sequence))
      ev-sequence
        (assign exp (op first-exp) (reg unev))
        (test (op last-exp?) (reg unev))
        (branch (label ev-sequence-last-exp))
        (save unev)
        (save env)
        (assign continue
                (label ev-sequence-continue))
        (goto (label eval-dispatch))
      ev-sequence-continue
        (restore env)
        (restore unev)
        (assign unev
                (op rest-exps)
                (reg unev))
        (goto (label ev-sequence))
      ev-sequence-last-exp
        (restore continue)
        (goto (label eval-dispatch))
      ev-if
        (save exp)   ; save expression for later
        (save env)
        (save continue)
        (assign continue (label ev-if-decide))
        (assign exp (op if-predicate) (reg exp))
        ; evaluate the predicate:
        (goto (label eval-dispatch))
      ev-if-decide
        (restore continue)
        (restore env)
        (restore exp)
        (test (op true?) (reg val))
        (branch (label ev-if-consequent))
      ev-if-alternative
        (assign exp (op if-alternative) (reg exp))
        (goto (label eval-dispatch))
      ev-if-consequent
        (assign exp (op if-consequent) (reg exp))
        (goto (label eval-dispatch))
      ;; 5.23
      ev-cond
        (assign exp (op cond->if) (reg exp))
        (goto (label eval-dispatch))
      ev-let
        (assign exp (op let->combination) (reg exp))
        (goto (label eval-dispatch))
      ;; 5.24
      ev-p-cond
        (assign unev (op cond-clauses) (reg exp)) ; Put all clauses in unev reg.
        ;(goto (label ev-p-cond-clauses)) ; Not strict necessary.
      ev-p-cond-clauses
        (test (op no-clauses?) (reg unev))
        (branch (label ev-p-cond-empty))
        (save unev)
        (assign exp (op first-clause) (reg unev)) ; Look at first clause.
        (save exp) ; Save clause (predicate & action) so we can access action later.
        (save continue) ; Save original continue passed to topmost eval.
        (assign continue (label ev-p-cond-decide))
        (assign exp (op cond-predicate) (reg exp))
        (test (op is-else?) (reg exp))     ; If predicate is an else,
        (assign val (const true))          ; we treat it as true
        (branch (label ev-p-cond-decide))  ; and proceed as usual.
        (goto (label eval-dispatch)) ; Eval what's in exp (the predicate).
      ev-p-cond-decide
        (restore continue) ; Original continue passed to topmost eval.
        (restore exp) ; The just evaluated clause.
        (restore unev) ; All the clauses at first.
        (test (op true?) (reg val))
        (branch (label ev-p-cond-done))
        (assign unev (op rest-clauses) (reg unev)) ; Remove head of unev clauses.
        (goto (label ev-p-cond-clauses))
      ev-p-cond-empty
        (assign val (const false)) ; Returns false if no condition satisfied.
        (goto (reg continue))
      ev-p-cond-done
        (save continue)
        (assign unev (op cond-actions) (reg exp))
        (goto (label ev-sequence))
        
      ev-assignment
        (assign unev 
                (op assignment-variable)
                (reg exp))
        (save unev)   ; save variable for later
        (assign exp
                (op assignment-value)
                (reg exp))
        (save env)
        (save continue)
        (assign continue
                (label ev-assignment-1))
        ; evaluate the assignment value:
        (goto (label eval-dispatch))  
      ev-assignment-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform (op set-variable-value!)
                 (reg unev)
                 (reg val)
                 (reg env))
        (assign val
                (const ok))
        (goto (reg continue))
      ev-definition
        (assign unev 
                (op definition-variable)
                (reg exp))
        (save unev)   ; save variable for later
        (assign exp 
                (op definition-value)
                (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-definition-1))
        ; evaluate the definition value:
        (goto (label eval-dispatch))  
      ev-definition-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform (op define-variable!)
                 (reg unev)
                 (reg val)
                 (reg env))
        (assign val (const ok))
        (goto (reg continue))
      done)))

;;; Tests

(define the-global-environment (setup-environment))
(set-register-contents! scheme-machine 'env (get-global-environment))
;(scheme-machine 'trace-on)

(start scheme-machine)