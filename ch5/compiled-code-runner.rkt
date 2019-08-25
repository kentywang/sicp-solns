#lang sicp

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
                (make-primitive-exp 
                 e machine labels))
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
        (error "Unbound variable" var)
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

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	;;above from book -- here are some more
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '= =)
	(list '/ /)
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

;;; For running compiled code, the register needs these primitive ops:

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) 
  (cadr c-proc))
(define (compiled-procedure-env c-proc)
  (caddr c-proc))

;;; The full scheme machine
(define scheme-machine
  (make-machine
    '(exp env val continue proc argl unev arg1 arg2)
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
            (list 'make-compiled-procedure make-compiled-procedure)
            (list 'compiled-procedure-env compiled-procedure-env)
            (list 'compiled-procedure-entry compiled-procedure-entry)
            (list 'list list)
            (list 'car car)
            (list 'cdr cdr)
            (list 'cons cons)
            (list 'null? null?)
            (list 'false? false?)
            (list 'true? true?)
            (list '+ +)
            (list '- -)
            (list '* *)
            (list '= =)
            (list '/ /)
            (list '> >)
            (list '< <)
            (list 'display (lambda x (apply display x) (newline)))
            )
    '(

      (assign val (op make-compiled-procedure) (label entry1) (reg env))
      (goto (label after-lambda2))
      entry1
      (assign env (op compiled-procedure-env) (reg proc))
      (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
      (assign arg1 (op lookup-variable-value) (const n) (reg env))
      (assign arg2 (const 1))
      (assign val (op =) (reg arg1) (reg arg2))
      (test (op false?) (reg val))
      (branch (label false-branch4))
      true-branch3
      (assign val (const 1))
      (goto (reg continue))
      false-branch4
      (save continue)
      (save env)
      (assign proc (op lookup-variable-value) (const factorial) (reg env))
      (assign arg1 (op lookup-variable-value) (const n) (reg env))
      (assign arg2 (const 1))
      (assign val (op -) (reg arg1) (reg arg2))
      (assign argl (op list) (reg val))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch6))
      compiled-branch7
      (assign continue (label proc-return9))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
      proc-return9
      (assign arg1 (reg val))
      (goto (label after-call8))
      primitive-branch6
      (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call8
      (restore env)
      (assign arg2 (op lookup-variable-value) (const n) (reg env))
      (assign val (op *) (reg arg1) (reg arg2))
      (restore continue)
      (goto (reg continue))
      after-if5
      after-lambda2
      (perform (op define-variable!) (const factorial) (reg val) (reg env))
      (assign val (const ok))
  
      ;; (factorial 4)
      (assign proc (op lookup-variable-value) (const factorial) (reg env))
      (assign val (const 7))
      (assign argl (op list) (reg val))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch10))
    compiled-branch11
      (assign continue (label after-call12))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
    primitive-branch10
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call12
      
      )))

;;; Tests

(define the-global-environment (setup-environment))
(set-register-contents! scheme-machine 'env (get-global-environment))
;(scheme-machine 'trace-on)

(start scheme-machine)

(get-register-contents scheme-machine 'val)