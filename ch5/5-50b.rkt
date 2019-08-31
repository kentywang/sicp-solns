#lang sicp

;; Observations:

;; There's primitive procedures, which are what compound procedures
;; in the implemented Scheme are build with.

;; These will need to be loaded into the env.

;; There's primitive operations, which is what the register machines use
;; to execute instructions, though they may be simulated with Scheme
;; procedures behind the scenes.

;; These will need to be installed in the machine (via install-operations).

;; I also ran into the same problem that the book brought up before:
;; map can't be a primitive procedure because the native Scheme map procedure
;; requires its first argument be a native procedure, but it'll be a compiled
;; procedure (that looks like "(compiled-procedure <entry> <env>)") instead.

;; A new change we need to do now is, similar to the map issue above, to
;; create an interpreted version of the apply procedure. We did this in the
;; original metacircular evaluator as "apply-in-underlying-scheme", but we'll
;; need to do it on another layer now (what layer? why?)

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

        ;; 5.50
        (list 'read read)
        (list 'display display)
        (list 'newline newline)
        (list 'apply apply)
        (list 'error error)
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list 'caadr caadr)
        (list 'cddr cddr)
        (list 'cdddr cdddr)
        (list 'cdadr cdadr)
        (list 'cadddr cadddr)
        (list 'eq? eq?)
        (list 'not not)
        (list 'length length)
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        (list 'list list)
        (list 'number? number?)
        (list 'string? string?)
        (list 'pair? pair?)
        (list 'symbol? symbol?)
       
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

;;; 5.39

;;; Constructor

(define (make-lexical-addr frame-no displacement-no)
  (cons frame-no displacement-no))

;;; Selectors

(define (frame-no addr) (car addr))
(define (displacement-no addr) (cdr addr))

;;; Main

(define (lexical-address-lookup addr env)
  (let* ((frame (list-ref env (frame-no addr)))
         (val (list-ref (frame-values frame) (displacement-no addr))))
    (if (eq? val '*unassigned*)
        (error "Scanned-out internal definitions not allowed")
        val)))

(define (lexical-address-set! addr val env)
  (define (enumerate-times n lst)
    (if (= n 0)
        lst
        (enumerate-times (- n 1) (cdr lst))))
  (let ((frame (list-ref env (frame-no addr))))
    (set-car! (enumerate-times
               (displacement-no addr)
               (frame-values frame))
              val)))

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
            ;; Helper
            (list 'display (lambda (x)
                             (newline)(newline)
                             (display "PRINT: ")(display x)
                             (newline)(newline)))
            )
    '(


     (assign val (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))
  entry1
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (f args)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const apply) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const args) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const primitive-implementation) (reg env))
  (assign val (op lookup-variable-value) (const f) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch3))
  compiled-branch4
  (assign continue (label after-call5))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch3
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call5
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch6))
  compiled-branch7
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch6
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call8
  after-lambda2
  (perform (op define-variable!) (const apply-in-underlying-scheme) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry9) (reg env))
  (goto (label after-lambda10))
  entry9
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (f lst)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const null?) (reg env))
  (assign val (op lookup-variable-value) (const lst) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch14))
  compiled-branch15
  (assign continue (label after-call16))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch14
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call16
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch12))
  true-branch11
  (assign val (op lookup-variable-value) (const lst) (reg env))
  (goto (reg continue))
  false-branch12
  (assign proc (op lookup-variable-value) (const cons) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const map) (reg env))
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const lst) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch23))
  compiled-branch24
  (assign continue (label after-call25))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch23
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call25
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const f) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch26))
  compiled-branch27
  (assign continue (label after-call28))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch26
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call28
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const car) (reg env))
  (assign val (op lookup-variable-value) (const lst) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch17))
  compiled-branch18
  (assign continue (label after-call19))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch17
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call19
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch20))
  compiled-branch21
  (assign continue (label after-call22))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch20
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call22
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch29))
  compiled-branch30
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch29
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call31
  after-if13
  after-lambda10
  (perform (op define-variable!) (const map) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry32) (reg env))
  (goto (label after-lambda33))
  entry32
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp env)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const self-evaluating?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch37))
  compiled-branch38
  (assign continue (label after-call39))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch37
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call39
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch35))
  true-branch34
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (goto (reg continue))
  false-branch35
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const variable?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch43))
  compiled-branch44
  (assign continue (label after-call45))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch43
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call45
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch41))
  true-branch40
  (assign proc (op lookup-variable-value) (const lookup-variable-value) (reg env))
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch46))
  compiled-branch47
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch46
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call48
  false-branch41
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const quoted?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch52))
  compiled-branch53
  (assign continue (label after-call54))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch52
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call54
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch50))
  true-branch49
  (assign proc (op lookup-variable-value) (const text-of-quotation) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch55))
  compiled-branch56
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch55
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call57
  false-branch50
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const assignment?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch61))
  compiled-branch62
  (assign continue (label after-call63))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch61
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call63
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch59))
  true-branch58
  (assign proc (op lookup-variable-value) (const eval-assignment) (reg env))
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch64))
  compiled-branch65
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch64
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call66
  false-branch59
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const definition?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch70))
  compiled-branch71
  (assign continue (label after-call72))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch70
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call72
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch68))
  true-branch67
  (assign proc (op lookup-variable-value) (const eval-definition) (reg env))
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch73))
  compiled-branch74
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch73
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call75
  false-branch68
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const if?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch79))
  compiled-branch80
  (assign continue (label after-call81))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch79
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call81
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch77))
  true-branch76
  (assign proc (op lookup-variable-value) (const eval-if) (reg env))
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch82))
  compiled-branch83
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch82
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call84
  false-branch77
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const lambda?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch88))
  compiled-branch89
  (assign continue (label after-call90))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch88
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call90
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch86))
  true-branch85
  (assign proc (op lookup-variable-value) (const make-procedure) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save env)
  (save argl)
  (assign proc (op lookup-variable-value) (const lambda-body) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch94))
  compiled-branch95
  (assign continue (label after-call96))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch94
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call96
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const lambda-parameters) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch91))
  compiled-branch92
  (assign continue (label after-call93))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch91
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call93
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch97))
  compiled-branch98
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch97
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call99
  false-branch86
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const begin?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch103))
  compiled-branch104
  (assign continue (label after-call105))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch103
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call105
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch101))
  true-branch100
  (assign proc (op lookup-variable-value) (const eval-sequence) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const begin-actions) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch106))
  compiled-branch107
  (assign continue (label after-call108))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch106
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call108
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch109))
  compiled-branch110
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch109
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call111
  false-branch101
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const cond?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch115))
  compiled-branch116
  (assign continue (label after-call117))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch115
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call117
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch113))
  true-branch112
  (assign proc (op lookup-variable-value) (const eval) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const cond->if) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch118))
  compiled-branch119
  (assign continue (label after-call120))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch118
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call120
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch121))
  compiled-branch122
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch121
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call123
  false-branch113
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const application?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch127))
  compiled-branch128
  (assign continue (label after-call129))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch127
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call129
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch125))
  true-branch124
  (assign proc (op lookup-variable-value) (const apply-in-interpreted-scheme) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const list-of-values) (reg env))
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const operands) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch136))
  compiled-branch137
  (assign continue (label after-call138))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch136
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call138
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch139))
  compiled-branch140
  (assign continue (label after-call141))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch139
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call141
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const eval) (reg env))
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const operator) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch130))
  compiled-branch131
  (assign continue (label after-call132))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch130
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call132
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch133))
  compiled-branch134
  (assign continue (label after-call135))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch133
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call135
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch142))
  compiled-branch143
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch142
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call144
  false-branch125
  (assign proc (op lookup-variable-value) (const error) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const "Unknown expression\n                 type: EVAL"))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch145))
  compiled-branch146
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch145
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call147
  after-if126
  after-if114
  after-if102
  after-if87
  after-if78
  after-if69
  after-if60
  after-if51
  after-if42
  after-if36
  after-lambda33
  (perform (op define-variable!) (const eval) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry148) (reg env))
  (goto (label after-lambda149))
  entry148
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (procedure arguments)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const primitive-procedure?) (reg env))
  (assign val (op lookup-variable-value) (const procedure) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch153))
  compiled-branch154
  (assign continue (label after-call155))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch153
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call155
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch151))
  true-branch150
  (assign proc (op lookup-variable-value) (const apply-primitive-procedure) (reg env))
  (assign val (op lookup-variable-value) (const arguments) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const procedure) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch156))
  compiled-branch157
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch156
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call158
  false-branch151
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const compound-procedure?) (reg env))
  (assign val (op lookup-variable-value) (const procedure) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch162))
  compiled-branch163
  (assign continue (label after-call164))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch162
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call164
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch160))
  true-branch159
  (assign proc (op lookup-variable-value) (const eval-sequence) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const extend-environment) (reg env))
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const procedure-environment) (reg env))
  (assign val (op lookup-variable-value) (const procedure) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch171))
  compiled-branch172
  (assign continue (label after-call173))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch171
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call173
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const arguments) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (save argl)
  (assign proc (op lookup-variable-value) (const procedure-parameters) (reg env))
  (assign val (op lookup-variable-value) (const procedure) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch168))
  compiled-branch169
  (assign continue (label after-call170))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch168
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call170
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch174))
  compiled-branch175
  (assign continue (label after-call176))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch174
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call176
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const procedure-body) (reg env))
  (assign val (op lookup-variable-value) (const procedure) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch165))
  compiled-branch166
  (assign continue (label after-call167))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch165
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call167
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch177))
  compiled-branch178
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch177
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call179
  false-branch160
  (assign proc (op lookup-variable-value) (const error) (reg env))
  (assign val (op lookup-variable-value) (const procedure) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const "Unknown procedure\n                 type: APPLY"))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch180))
  compiled-branch181
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch180
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call182
  after-if161
  after-if152
  after-lambda149
  (perform (op define-variable!) (const apply-in-interpreted-scheme) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry183) (reg env))
  (goto (label after-lambda184))
  entry183
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exps env)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const no-operands?) (reg env))
  (assign val (op lookup-variable-value) (const exps) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch188))
  compiled-branch189
  (assign continue (label after-call190))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch188
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call190
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch186))
  true-branch185
  (assign val (const ()))
  (goto (reg continue))
  false-branch186
  (assign proc (op lookup-variable-value) (const cons) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const list-of-values) (reg env))
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const rest-operands) (reg env))
  (assign val (op lookup-variable-value) (const exps) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch197))
  compiled-branch198
  (assign continue (label after-call199))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch197
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call199
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch200))
  compiled-branch201
  (assign continue (label after-call202))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch200
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call202
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const eval) (reg env))
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const first-operand) (reg env))
  (assign val (op lookup-variable-value) (const exps) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch191))
  compiled-branch192
  (assign continue (label after-call193))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch191
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call193
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch194))
  compiled-branch195
  (assign continue (label after-call196))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch194
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call196
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch203))
  compiled-branch204
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch203
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call205
  after-if187
  after-lambda184
  (perform (op define-variable!) (const list-of-values) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry206) (reg env))
  (goto (label after-lambda207))
  entry206
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp env)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const true?) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const eval) (reg env))
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const if-predicate) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch211))
  compiled-branch212
  (assign continue (label after-call213))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch211
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call213
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch214))
  compiled-branch215
  (assign continue (label after-call216))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch214
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call216
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch217))
  compiled-branch218
  (assign continue (label after-call219))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch217
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call219
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch209))
  true-branch208
  (assign proc (op lookup-variable-value) (const eval) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const if-consequent) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch220))
  compiled-branch221
  (assign continue (label after-call222))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch220
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call222
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch223))
  compiled-branch224
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch223
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call225
  false-branch209
  (assign proc (op lookup-variable-value) (const eval) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const if-alternative) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch226))
  compiled-branch227
  (assign continue (label after-call228))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch226
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call228
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch229))
  compiled-branch230
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch229
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call231
  after-if210
  after-lambda207
  (perform (op define-variable!) (const eval-if) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry232) (reg env))
  (goto (label after-lambda233))
  entry232
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exps env)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const last-exp?) (reg env))
  (assign val (op lookup-variable-value) (const exps) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch237))
  compiled-branch238
  (assign continue (label after-call239))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch237
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call239
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch235))
  true-branch234
  (assign proc (op lookup-variable-value) (const eval) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const first-exp) (reg env))
  (assign val (op lookup-variable-value) (const exps) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch240))
  compiled-branch241
  (assign continue (label after-call242))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch240
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call242
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch243))
  compiled-branch244
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch243
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call245
  false-branch235
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const eval) (reg env))
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const first-exp) (reg env))
  (assign val (op lookup-variable-value) (const exps) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch246))
  compiled-branch247
  (assign continue (label after-call248))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch246
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call248
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch249))
  compiled-branch250
  (assign continue (label after-call251))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch249
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call251
  (restore env)
  (restore continue)
  (assign proc (op lookup-variable-value) (const eval-sequence) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const rest-exps) (reg env))
  (assign val (op lookup-variable-value) (const exps) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch252))
  compiled-branch253
  (assign continue (label after-call254))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch252
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call254
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch255))
  compiled-branch256
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch255
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call257
  after-if236
  after-lambda233
  (perform (op define-variable!) (const eval-sequence) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry258) (reg env))
  (goto (label after-lambda259))
  entry258
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp env)) (reg argl) (reg env))
  (save continue)
  (assign proc (op lookup-variable-value) (const set-variable-value!) (reg env))
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save env)
  (save argl)
  (assign proc (op lookup-variable-value) (const eval) (reg env))
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const assignment-value) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch263))
  compiled-branch264
  (assign continue (label after-call265))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch263
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call265
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch266))
  compiled-branch267
  (assign continue (label after-call268))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch266
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call268
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const assignment-variable) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch260))
  compiled-branch261
  (assign continue (label after-call262))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch260
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call262
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch269))
  compiled-branch270
  (assign continue (label after-call271))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch269
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call271
  (restore continue)
  (assign val (const ok))
  (goto (reg continue))
  after-lambda259
  (perform (op define-variable!) (const eval-assignment) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry272) (reg env))
  (goto (label after-lambda273))
  entry272
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp env)) (reg argl) (reg env))
  (save continue)
  (assign proc (op lookup-variable-value) (const define-variable!) (reg env))
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save env)
  (save argl)
  (assign proc (op lookup-variable-value) (const eval) (reg env))
  (save proc)
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const definition-value) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch277))
  compiled-branch278
  (assign continue (label after-call279))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch277
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call279
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch280))
  compiled-branch281
  (assign continue (label after-call282))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch280
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call282
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const definition-variable) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch274))
  compiled-branch275
  (assign continue (label after-call276))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch274
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call276
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch283))
  compiled-branch284
  (assign continue (label after-call285))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch283
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call285
  (restore continue)
  (assign val (const ok))
  (goto (reg continue))
  after-lambda273
  (perform (op define-variable!) (const eval-definition) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry286) (reg env))
  (goto (label after-lambda287))
  entry286
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const number?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch291))
  compiled-branch292
  (assign continue (label after-call293))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch291
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call293
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch289))
  true-branch288
  (assign val (op lookup-variable-value) (const true) (reg env))
  (goto (reg continue))
  false-branch289
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const string?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch297))
  compiled-branch298
  (assign continue (label after-call299))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch297
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call299
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch295))
  true-branch294
  (assign val (op lookup-variable-value) (const true) (reg env))
  (goto (reg continue))
  false-branch295
  (assign val (op lookup-variable-value) (const false) (reg env))
  (goto (reg continue))
  after-if296
  after-if290
  after-lambda287
  (perform (op define-variable!) (const self-evaluating?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry300) (reg env))
  (goto (label after-lambda301))
  entry300
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const symbol?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch302))
  compiled-branch303
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch302
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call304
  after-lambda301
  (perform (op define-variable!) (const variable?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry305) (reg env))
  (goto (label after-lambda306))
  entry305
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const tagged-list?) (reg env))
  (assign val (const quote))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch307))
  compiled-branch308
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch307
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call309
  after-lambda306
  (perform (op define-variable!) (const quoted?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry310) (reg env))
  (goto (label after-lambda311))
  entry310
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cadr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch312))
  compiled-branch313
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch312
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call314
  after-lambda311
  (perform (op define-variable!) (const text-of-quotation) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry315) (reg env))
  (goto (label after-lambda316))
  entry315
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp tag)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const pair?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch320))
  compiled-branch321
  (assign continue (label after-call322))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch320
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call322
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch318))
  true-branch317
  (assign proc (op lookup-variable-value) (const eq?) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const tag) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const car) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch323))
  compiled-branch324
  (assign continue (label after-call325))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch323
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call325
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch326))
  compiled-branch327
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch326
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call328
  false-branch318
  (assign val (op lookup-variable-value) (const false) (reg env))
  (goto (reg continue))
  after-if319
  after-lambda316
  (perform (op define-variable!) (const tagged-list?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry329) (reg env))
  (goto (label after-lambda330))
  entry329
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const tagged-list?) (reg env))
  (assign val (const set!))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch331))
  compiled-branch332
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch331
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call333
  after-lambda330
  (perform (op define-variable!) (const assignment?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry334) (reg env))
  (goto (label after-lambda335))
  entry334
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cadr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch336))
  compiled-branch337
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch336
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call338
  after-lambda335
  (perform (op define-variable!) (const assignment-variable) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry339) (reg env))
  (goto (label after-lambda340))
  entry339
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const caddr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch341))
  compiled-branch342
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch341
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call343
  after-lambda340
  (perform (op define-variable!) (const assignment-value) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry344) (reg env))
  (goto (label after-lambda345))
  entry344
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const tagged-list?) (reg env))
  (assign val (const define))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch346))
  compiled-branch347
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch346
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call348
  after-lambda345
  (perform (op define-variable!) (const definition?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry349) (reg env))
  (goto (label after-lambda350))
  entry349
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const symbol?) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const cadr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch354))
  compiled-branch355
  (assign continue (label after-call356))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch354
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call356
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch357))
  compiled-branch358
  (assign continue (label after-call359))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch357
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call359
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch352))
  true-branch351
  (assign proc (op lookup-variable-value) (const cadr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch360))
  compiled-branch361
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch360
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call362
  false-branch352
  (assign proc (op lookup-variable-value) (const caadr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch363))
  compiled-branch364
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch363
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call365
  after-if353
  after-lambda350
  (perform (op define-variable!) (const definition-variable) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry366) (reg env))
  (goto (label after-lambda367))
  entry366
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const symbol?) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const cadr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch371))
  compiled-branch372
  (assign continue (label after-call373))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch371
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call373
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch374))
  compiled-branch375
  (assign continue (label after-call376))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch374
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call376
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch369))
  true-branch368
  (assign proc (op lookup-variable-value) (const caddr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch377))
  compiled-branch378
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch377
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call379
  false-branch369
  (assign proc (op lookup-variable-value) (const make-lambda) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const cddr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch383))
  compiled-branch384
  (assign continue (label after-call385))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch383
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call385
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const cdadr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch380))
  compiled-branch381
  (assign continue (label after-call382))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch380
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call382
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch386))
  compiled-branch387
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch386
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call388
  after-if370
  after-lambda367
  (perform (op define-variable!) (const definition-value) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry389) (reg env))
  (goto (label after-lambda390))
  entry389
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const tagged-list?) (reg env))
  (assign val (const lambda))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch391))
  compiled-branch392
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch391
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call393
  after-lambda390
  (perform (op define-variable!) (const lambda?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry394) (reg env))
  (goto (label after-lambda395))
  entry394
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cadr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch396))
  compiled-branch397
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch396
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call398
  after-lambda395
  (perform (op define-variable!) (const lambda-parameters) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry399) (reg env))
  (goto (label after-lambda400))
  entry399
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cddr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch401))
  compiled-branch402
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch401
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call403
  after-lambda400
  (perform (op define-variable!) (const lambda-body) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry404) (reg env))
  (goto (label after-lambda405))
  entry404
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (parameters body)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cons) (reg env))
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const cons) (reg env))
  (assign val (op lookup-variable-value) (const body) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const parameters) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch406))
  compiled-branch407
  (assign continue (label after-call408))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch406
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call408
  (assign argl (op list) (reg val))
  (assign val (const lambda))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch409))
  compiled-branch410
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch409
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call411
  after-lambda405
  (perform (op define-variable!) (const make-lambda) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry412) (reg env))
  (goto (label after-lambda413))
  entry412
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const tagged-list?) (reg env))
  (assign val (const if))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch414))
  compiled-branch415
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch414
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call416
  after-lambda413
  (perform (op define-variable!) (const if?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry417) (reg env))
  (goto (label after-lambda418))
  entry417
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cadr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch419))
  compiled-branch420
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch419
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call421
  after-lambda418
  (perform (op define-variable!) (const if-predicate) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry422) (reg env))
  (goto (label after-lambda423))
  entry422
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const caddr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch424))
  compiled-branch425
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch424
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call426
  after-lambda423
  (perform (op define-variable!) (const if-consequent) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry427) (reg env))
  (goto (label after-lambda428))
  entry427
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const not) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const null?) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const cdddr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch432))
  compiled-branch433
  (assign continue (label after-call434))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch432
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call434
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch435))
  compiled-branch436
  (assign continue (label after-call437))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch435
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call437
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch438))
  compiled-branch439
  (assign continue (label after-call440))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch438
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call440
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch430))
  true-branch429
  (assign proc (op lookup-variable-value) (const cadddr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch441))
  compiled-branch442
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch441
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call443
  false-branch430
  (assign val (const false))
  (goto (reg continue))
  after-if431
  after-lambda428
  (perform (op define-variable!) (const if-alternative) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry444) (reg env))
  (goto (label after-lambda445))
  entry444
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (predicate consequent alternative)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (assign val (op lookup-variable-value) (const alternative) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const consequent) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (op lookup-variable-value) (const predicate) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (const if))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch446))
  compiled-branch447
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch446
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call448
  after-lambda445
  (perform (op define-variable!) (const make-if) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry449) (reg env))
  (goto (label after-lambda450))
  entry449
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const tagged-list?) (reg env))
  (assign val (const begin))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch451))
  compiled-branch452
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch451
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call453
  after-lambda450
  (perform (op define-variable!) (const begin?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry454) (reg env))
  (goto (label after-lambda455))
  entry454
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch456))
  compiled-branch457
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch456
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call458
  after-lambda455
  (perform (op define-variable!) (const begin-actions) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry459) (reg env))
  (goto (label after-lambda460))
  entry459
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (seq)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const null?) (reg env))
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const seq) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch461))
  compiled-branch462
  (assign continue (label after-call463))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch461
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call463
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch464))
  compiled-branch465
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch464
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call466
  after-lambda460
  (perform (op define-variable!) (const last-exp?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry467) (reg env))
  (goto (label after-lambda468))
  entry467
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (seq)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const car) (reg env))
  (assign val (op lookup-variable-value) (const seq) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch469))
  compiled-branch470
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch469
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call471
  after-lambda468
  (perform (op define-variable!) (const first-exp) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry472) (reg env))
  (goto (label after-lambda473))
  entry472
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (seq)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const seq) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch474))
  compiled-branch475
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch474
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call476
  after-lambda473
  (perform (op define-variable!) (const rest-exps) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry477) (reg env))
  (goto (label after-lambda478))
  entry477
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (seq)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const null?) (reg env))
  (assign val (op lookup-variable-value) (const seq) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch482))
  compiled-branch483
  (assign continue (label after-call484))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch482
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call484
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch480))
  true-branch479
  (assign val (op lookup-variable-value) (const seq) (reg env))
  (goto (reg continue))
  false-branch480
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const last-exp?) (reg env))
  (assign val (op lookup-variable-value) (const seq) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch488))
  compiled-branch489
  (assign continue (label after-call490))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch488
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call490
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch486))
  true-branch485
  (assign proc (op lookup-variable-value) (const first-exp) (reg env))
  (assign val (op lookup-variable-value) (const seq) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch491))
  compiled-branch492
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch491
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call493
  false-branch486
  (assign proc (op lookup-variable-value) (const make-begin) (reg env))
  (assign val (op lookup-variable-value) (const seq) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch494))
  compiled-branch495
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch494
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call496
  after-if487
  after-if481
  after-lambda478
  (perform (op define-variable!) (const sequence->exp) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry497) (reg env))
  (goto (label after-lambda498))
  entry497
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (seq)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cons) (reg env))
  (assign val (op lookup-variable-value) (const seq) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const begin))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch499))
  compiled-branch500
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch499
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call501
  after-lambda498
  (perform (op define-variable!) (const make-begin) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry502) (reg env))
  (goto (label after-lambda503))
  entry502
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const pair?) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch504))
  compiled-branch505
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch504
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call506
  after-lambda503
  (perform (op define-variable!) (const application?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry507) (reg env))
  (goto (label after-lambda508))
  entry507
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const car) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch509))
  compiled-branch510
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch509
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call511
  after-lambda508
  (perform (op define-variable!) (const operator) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry512) (reg env))
  (goto (label after-lambda513))
  entry512
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch514))
  compiled-branch515
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch514
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call516
  after-lambda513
  (perform (op define-variable!) (const operands) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry517) (reg env))
  (goto (label after-lambda518))
  entry517
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (ops)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const null?) (reg env))
  (assign val (op lookup-variable-value) (const ops) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch519))
  compiled-branch520
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch519
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call521
  after-lambda518
  (perform (op define-variable!) (const no-operands?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry522) (reg env))
  (goto (label after-lambda523))
  entry522
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (ops)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const car) (reg env))
  (assign val (op lookup-variable-value) (const ops) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch524))
  compiled-branch525
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch524
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call526
  after-lambda523
  (perform (op define-variable!) (const first-operand) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry527) (reg env))
  (goto (label after-lambda528))
  entry527
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (ops)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const ops) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch529))
  compiled-branch530
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch529
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call531
  after-lambda528
  (perform (op define-variable!) (const rest-operands) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry532) (reg env))
  (goto (label after-lambda533))
  entry532
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const tagged-list?) (reg env))
  (assign val (const cond))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch534))
  compiled-branch535
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch534
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call536
  after-lambda533
  (perform (op define-variable!) (const cond?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry537) (reg env))
  (goto (label after-lambda538))
  entry537
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch539))
  compiled-branch540
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch539
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call541
  after-lambda538
  (perform (op define-variable!) (const cond-clauses) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry542) (reg env))
  (goto (label after-lambda543))
  entry542
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (clause)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const eq?) (reg env))
  (save continue)
  (save proc)
  (assign val (const else))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const cond-predicate) (reg env))
  (assign val (op lookup-variable-value) (const clause) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch544))
  compiled-branch545
  (assign continue (label after-call546))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch544
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call546
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch547))
  compiled-branch548
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch547
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call549
  after-lambda543
  (perform (op define-variable!) (const cond-else-clause?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry550) (reg env))
  (goto (label after-lambda551))
  entry550
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (clause)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const car) (reg env))
  (assign val (op lookup-variable-value) (const clause) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch552))
  compiled-branch553
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch552
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call554
  after-lambda551
  (perform (op define-variable!) (const cond-predicate) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry555) (reg env))
  (goto (label after-lambda556))
  entry555
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (clause)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const clause) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch557))
  compiled-branch558
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch557
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call559
  after-lambda556
  (perform (op define-variable!) (const cond-actions) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry560) (reg env))
  (goto (label after-lambda561))
  entry560
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (exp)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const expand-clauses) (reg env))
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const cond-clauses) (reg env))
  (assign val (op lookup-variable-value) (const exp) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch562))
  compiled-branch563
  (assign continue (label after-call564))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch562
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call564
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch565))
  compiled-branch566
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch565
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call567
  after-lambda561
  (perform (op define-variable!) (const cond->if) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry568) (reg env))
  (goto (label after-lambda569))
  entry568
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (clauses)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const null?) (reg env))
  (assign val (op lookup-variable-value) (const clauses) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch573))
  compiled-branch574
  (assign continue (label after-call575))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch573
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call575
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch571))
  true-branch570
  (assign val (const false))
  (goto (reg continue))
  false-branch571
  (assign proc (op make-compiled-procedure) (label entry576) (reg env))
  (goto (label after-lambda577))
  entry576
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (first rest)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const cond-else-clause?) (reg env))
  (assign val (op lookup-variable-value) (const first) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch581))
  compiled-branch582
  (assign continue (label after-call583))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch581
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call583
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch579))
  true-branch578
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const null?) (reg env))
  (assign val (op lookup-variable-value) (const rest) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch587))
  compiled-branch588
  (assign continue (label after-call589))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch587
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call589
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch585))
  true-branch584
  (assign proc (op lookup-variable-value) (const sequence->exp) (reg env))
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const cond-actions) (reg env))
  (assign val (op lookup-variable-value) (const first) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch590))
  compiled-branch591
  (assign continue (label after-call592))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch590
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call592
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch593))
  compiled-branch594
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch593
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call595
  false-branch585
  (assign proc (op lookup-variable-value) (const error) (reg env))
  (assign val (op lookup-variable-value) (const clauses) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const "ELSE clause isn't\n                        last: COND->IF"))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch596))
  compiled-branch597
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch596
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call598
  after-if586
  false-branch579
  (assign proc (op lookup-variable-value) (const make-if) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const expand-clauses) (reg env))
  (assign val (op lookup-variable-value) (const rest) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch608))
  compiled-branch609
  (assign continue (label after-call610))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch608
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call610
  (assign argl (op list) (reg val))
  (restore env)
  (save env)
  (save argl)
  (assign proc (op lookup-variable-value) (const sequence->exp) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const cond-actions) (reg env))
  (assign val (op lookup-variable-value) (const first) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch602))
  compiled-branch603
  (assign continue (label after-call604))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch602
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call604
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch605))
  compiled-branch606
  (assign continue (label after-call607))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch605
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call607
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const cond-predicate) (reg env))
  (assign val (op lookup-variable-value) (const first) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch599))
  compiled-branch600
  (assign continue (label after-call601))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch599
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call601
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch611))
  compiled-branch612
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch611
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call613
  after-if580
  after-lambda577
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const clauses) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch617))
  compiled-branch618
  (assign continue (label after-call619))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch617
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call619
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const car) (reg env))
  (assign val (op lookup-variable-value) (const clauses) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch614))
  compiled-branch615
  (assign continue (label after-call616))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch614
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call616
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch620))
  compiled-branch621
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch620
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call622
  after-if572
  after-lambda569
  (perform (op define-variable!) (const expand-clauses) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry623) (reg env))
  (goto (label after-lambda624))
  entry623
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const not) (reg env))
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const eq?) (reg env))
  (assign val (op lookup-variable-value) (const false) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch625))
  compiled-branch626
  (assign continue (label after-call627))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch625
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call627
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch628))
  compiled-branch629
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch628
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call630
  after-lambda624
  (perform (op define-variable!) (const true?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry631) (reg env))
  (goto (label after-lambda632))
  entry631
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const eq?) (reg env))
  (assign val (op lookup-variable-value) (const false) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch633))
  compiled-branch634
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch633
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call635
  after-lambda632
  (perform (op define-variable!) (const false?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry636) (reg env))
  (goto (label after-lambda637))
  entry636
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (parameters body env)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const body) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (op lookup-variable-value) (const parameters) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (const procedure))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch638))
  compiled-branch639
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch638
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call640
  after-lambda637
  (perform (op define-variable!) (const make-procedure) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry641) (reg env))
  (goto (label after-lambda642))
  entry641
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (p)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const tagged-list?) (reg env))
  (assign val (const procedure))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const p) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch643))
  compiled-branch644
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch643
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call645
  after-lambda642
  (perform (op define-variable!) (const compound-procedure?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry646) (reg env))
  (goto (label after-lambda647))
  entry646
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (p)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cadr) (reg env))
  (assign val (op lookup-variable-value) (const p) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch648))
  compiled-branch649
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch648
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call650
  after-lambda647
  (perform (op define-variable!) (const procedure-parameters) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry651) (reg env))
  (goto (label after-lambda652))
  entry651
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (p)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const caddr) (reg env))
  (assign val (op lookup-variable-value) (const p) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch653))
  compiled-branch654
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch653
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call655
  after-lambda652
  (perform (op define-variable!) (const procedure-body) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry656) (reg env))
  (goto (label after-lambda657))
  entry656
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (p)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cadddr) (reg env))
  (assign val (op lookup-variable-value) (const p) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch658))
  compiled-branch659
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch658
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call660
  after-lambda657
  (perform (op define-variable!) (const procedure-environment) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry661) (reg env))
  (goto (label after-lambda662))
  entry661
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (env)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch663))
  compiled-branch664
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch663
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call665
  after-lambda662
  (perform (op define-variable!) (const enclosing-environment) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry666) (reg env))
  (goto (label after-lambda667))
  entry666
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (env)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const car) (reg env))
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch668))
  compiled-branch669
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch668
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call670
  after-lambda667
  (perform (op define-variable!) (const first-frame) (reg val) (reg env))
  (assign val (const ok))
  (assign val (const ()))
  (perform (op define-variable!) (const the-empty-environment) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry671) (reg env))
  (goto (label after-lambda672))
  entry671
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (variables values)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cons) (reg env))
  (assign val (op lookup-variable-value) (const values) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const variables) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch673))
  compiled-branch674
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch673
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call675
  after-lambda672
  (perform (op define-variable!) (const make-frame) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry676) (reg env))
  (goto (label after-lambda677))
  entry676
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (frame)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const car) (reg env))
  (assign val (op lookup-variable-value) (const frame) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch678))
  compiled-branch679
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch678
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call680
  after-lambda677
  (perform (op define-variable!) (const frame-variables) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry681) (reg env))
  (goto (label after-lambda682))
  entry681
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (frame)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const frame) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch683))
  compiled-branch684
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch683
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call685
  after-lambda682
  (perform (op define-variable!) (const frame-values) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry686) (reg env))
  (goto (label after-lambda687))
  entry686
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (var val frame)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const set-car!) (reg env))
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const cons) (reg env))
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const car) (reg env))
  (assign val (op lookup-variable-value) (const frame) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch688))
  compiled-branch689
  (assign continue (label after-call690))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch688
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call690
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const var) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch691))
  compiled-branch692
  (assign continue (label after-call693))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch691
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call693
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const frame) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch694))
  compiled-branch695
  (assign continue (label after-call696))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch694
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call696
  (restore env)
  (restore continue)
  (assign proc (op lookup-variable-value) (const set-cdr!) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const cons) (reg env))
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const frame) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch697))
  compiled-branch698
  (assign continue (label after-call699))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch697
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call699
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const val) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch700))
  compiled-branch701
  (assign continue (label after-call702))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch700
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call702
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const frame) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch703))
  compiled-branch704
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch703
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call705
  after-lambda687
  (perform (op define-variable!) (const add-binding-to-frame!) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry706) (reg env))
  (goto (label after-lambda707))
  entry706
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (vars vals base-env)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const length) (reg env))
  (assign val (op lookup-variable-value) (const vals) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch714))
  compiled-branch715
  (assign continue (label after-call716))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch714
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call716
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const length) (reg env))
  (assign val (op lookup-variable-value) (const vars) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch711))
  compiled-branch712
  (assign continue (label after-call713))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch711
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call713
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch717))
  compiled-branch718
  (assign continue (label after-call719))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch717
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call719
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch709))
  true-branch708
  (assign proc (op lookup-variable-value) (const cons) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const base-env) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const make-frame) (reg env))
  (assign val (op lookup-variable-value) (const vals) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const vars) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch720))
  compiled-branch721
  (assign continue (label after-call722))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch720
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call722
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch723))
  compiled-branch724
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch723
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call725
  false-branch709
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const <) (reg env))
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const length) (reg env))
  (assign val (op lookup-variable-value) (const vals) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch732))
  compiled-branch733
  (assign continue (label after-call734))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch732
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call734
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const length) (reg env))
  (assign val (op lookup-variable-value) (const vars) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch729))
  compiled-branch730
  (assign continue (label after-call731))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch729
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call731
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch735))
  compiled-branch736
  (assign continue (label after-call737))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch735
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call737
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch727))
  true-branch726
  (assign proc (op lookup-variable-value) (const error) (reg env))
  (assign val (op lookup-variable-value) (const vals) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const vars) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (const "Too many arguments supplied"))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch738))
  compiled-branch739
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch738
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call740
  false-branch727
  (assign proc (op lookup-variable-value) (const error) (reg env))
  (assign val (op lookup-variable-value) (const vals) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const vars) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (const "Too few arguments supplied"))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch741))
  compiled-branch742
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch741
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call743
  after-if728
  after-if710
  after-lambda707
  (perform (op define-variable!) (const extend-environment) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry744) (reg env))
  (goto (label after-lambda745))
  entry744
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (var env)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry746) (reg env))
  (goto (label after-lambda747))
  entry746
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (vars vals)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const null?) (reg env))
  (assign val (op lookup-variable-value) (const vars) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch751))
  compiled-branch752
  (assign continue (label after-call753))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch751
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call753
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch749))
  true-branch748
  (assign proc (op lookup-variable-value) (const find-binding) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const enclosing-environment) (reg env))
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch754))
  compiled-branch755
  (assign continue (label after-call756))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch754
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call756
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const var) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch757))
  compiled-branch758
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch757
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call759
  false-branch749
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const eq?) (reg env))
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const car) (reg env))
  (assign val (op lookup-variable-value) (const vars) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch763))
  compiled-branch764
  (assign continue (label after-call765))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch763
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call765
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const var) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch766))
  compiled-branch767
  (assign continue (label after-call768))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch766
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call768
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch761))
  true-branch760
  (assign proc (op lookup-variable-value) (const cons) (reg env))
  (assign val (op lookup-variable-value) (const vals) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const vars) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch769))
  compiled-branch770
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch769
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call771
  false-branch761
  (assign proc (op lookup-variable-value) (const scan) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const vals) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch775))
  compiled-branch776
  (assign continue (label after-call777))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch775
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call777
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const vars) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch772))
  compiled-branch773
  (assign continue (label after-call774))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch772
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call774
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch778))
  compiled-branch779
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch778
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call780
  after-if762
  after-if750
  after-lambda747
  (perform (op define-variable!) (const scan) (reg val) (reg env))
  (assign val (const ok))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const eq?) (reg env))
  (assign val (op lookup-variable-value) (const the-empty-environment) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch784))
  compiled-branch785
  (assign continue (label after-call786))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch784
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call786
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch782))
  true-branch781
  (assign val (op lookup-variable-value) (const false) (reg env))
  (goto (reg continue))
  false-branch782
  (assign proc (op make-compiled-procedure) (label entry787) (reg env))
  (goto (label after-lambda788))
  entry787
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (frame)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const scan) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const frame-values) (reg env))
  (assign val (op lookup-variable-value) (const frame) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch792))
  compiled-branch793
  (assign continue (label after-call794))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch792
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call794
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const frame-variables) (reg env))
  (assign val (op lookup-variable-value) (const frame) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch789))
  compiled-branch790
  (assign continue (label after-call791))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch789
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call791
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch795))
  compiled-branch796
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch795
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call797
  after-lambda788
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const first-frame) (reg env))
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch798))
  compiled-branch799
  (assign continue (label after-call800))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch798
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call800
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch801))
  compiled-branch802
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch801
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call803
  after-if783
  after-lambda745
  (perform (op define-variable!) (const find-binding) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry804) (reg env))
  (goto (label after-lambda805))
  entry804
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (var env)) (reg argl) (reg env))
  (assign proc (op make-compiled-procedure) (label entry806) (reg env))
  (goto (label after-lambda807))
  entry806
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (binding)) (reg argl) (reg env))
  (assign val (op lookup-variable-value) (const binding) (reg env))
  (test (op false?) (reg val))
  (branch (label false-branch809))
  true-branch808
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const eq?) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const cadr) (reg env))
  (assign val (op lookup-variable-value) (const binding) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch814))
  compiled-branch815
  (assign continue (label after-call816))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch814
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call816
  (assign argl (op list) (reg val))
  (assign val (const *unassigned*))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch817))
  compiled-branch818
  (assign continue (label after-call819))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch817
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call819
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch812))
  true-branch811
  (assign proc (op lookup-variable-value) (const error) (reg env))
  (assign val (op lookup-variable-value) (const var) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const "Unassigned variable"))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch820))
  compiled-branch821
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch820
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call822
  false-branch812
  (assign proc (op lookup-variable-value) (const cadr) (reg env))
  (assign val (op lookup-variable-value) (const binding) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch823))
  compiled-branch824
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch823
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call825
  after-if813
  false-branch809
  (assign proc (op lookup-variable-value) (const error) (reg env))
  (assign val (op lookup-variable-value) (const var) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const "Unbound variable"))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch826))
  compiled-branch827
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch826
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call828
  after-if810
  after-lambda807
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const find-binding) (reg env))
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const var) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch829))
  compiled-branch830
  (assign continue (label after-call831))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch829
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call831
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch832))
  compiled-branch833
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch832
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call834
  after-lambda805
  (perform (op define-variable!) (const lookup-variable-value) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry835) (reg env))
  (goto (label after-lambda836))
  entry835
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (var val env)) (reg argl) (reg env))
  (assign proc (op make-compiled-procedure) (label entry837) (reg env))
  (goto (label after-lambda838))
  entry837
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (binding)) (reg argl) (reg env))
  (assign val (op lookup-variable-value) (const binding) (reg env))
  (test (op false?) (reg val))
  (branch (label false-branch840))
  true-branch839
  (assign proc (op lookup-variable-value) (const set-car!) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const val) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const binding) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch842))
  compiled-branch843
  (assign continue (label after-call844))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch842
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call844
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch845))
  compiled-branch846
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch845
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call847
  false-branch840
  (assign proc (op lookup-variable-value) (const error) (reg env))
  (assign val (op lookup-variable-value) (const var) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const "Unbound variable: SET!"))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch848))
  compiled-branch849
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch848
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call850
  after-if841
  after-lambda838
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const find-binding) (reg env))
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const var) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch851))
  compiled-branch852
  (assign continue (label after-call853))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch851
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call853
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch854))
  compiled-branch855
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch854
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call856
  after-lambda836
  (perform (op define-variable!) (const set-variable-value!) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry857) (reg env))
  (goto (label after-lambda858))
  entry857
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (var val env)) (reg argl) (reg env))
  (assign proc (op make-compiled-procedure) (label entry859) (reg env))
  (goto (label after-lambda860))
  entry859
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (frame)) (reg argl) (reg env))
  (assign proc (op make-compiled-procedure) (label entry861) (reg env))
  (goto (label after-lambda862))
  entry861
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (binding)) (reg argl) (reg env))
  (assign val (op lookup-variable-value) (const binding) (reg env))
  (test (op false?) (reg val))
  (branch (label false-branch864))
  true-branch863
  (assign proc (op lookup-variable-value) (const set-car!) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const val) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const cdr) (reg env))
  (assign val (op lookup-variable-value) (const binding) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch866))
  compiled-branch867
  (assign continue (label after-call868))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch866
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call868
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch869))
  compiled-branch870
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch869
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call871
  false-branch864
  (assign proc (op lookup-variable-value) (const add-binding-to-frame!) (reg env))
  (assign val (op lookup-variable-value) (const frame) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const val) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (op lookup-variable-value) (const var) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch872))
  compiled-branch873
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch872
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call874
  after-if865
  after-lambda862
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const find-binding) (reg env))
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (assign val (op lookup-variable-value) (const frame) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch875))
  compiled-branch876
  (assign continue (label after-call877))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch875
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call877
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const var) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch878))
  compiled-branch879
  (assign continue (label after-call880))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch878
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call880
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch881))
  compiled-branch882
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch881
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call883
  after-lambda860
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const first-frame) (reg env))
  (assign val (op lookup-variable-value) (const env) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch884))
  compiled-branch885
  (assign continue (label after-call886))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch884
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call886
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch887))
  compiled-branch888
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch887
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call889
  after-lambda858
  (perform (op define-variable!) (const define-variable!) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry890) (reg env))
  (goto (label after-lambda891))
  entry890
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (proc)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const tagged-list?) (reg env))
  (assign val (const primitive))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const proc) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch892))
  compiled-branch893
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch892
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call894
  after-lambda891
  (perform (op define-variable!) (const primitive-procedure?) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry895) (reg env))
  (goto (label after-lambda896))
  entry895
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (proc)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const cadr) (reg env))
  (assign val (op lookup-variable-value) (const proc) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch897))
  compiled-branch898
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch897
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call899
  after-lambda896
  (perform (op define-variable!) (const primitive-implementation) (reg val) (reg env))
  (assign val (const ok))
  (save env)
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (assign val (op lookup-variable-value) (const =) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const =))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch924))
  compiled-branch925
  (assign continue (label after-call926))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch924
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call926
  (assign argl (op list) (reg val))
  (restore env)
  (save env)
  (save argl)
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (assign val (op lookup-variable-value) (const /) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const /))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch921))
  compiled-branch922
  (assign continue (label after-call923))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch921
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call923
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore env)
  (save env)
  (save argl)
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (assign val (op lookup-variable-value) (const *) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const *))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch918))
  compiled-branch919
  (assign continue (label after-call920))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch918
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call920
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore env)
  (save env)
  (save argl)
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (assign val (op lookup-variable-value) (const -) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const -))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch915))
  compiled-branch916
  (assign continue (label after-call917))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch915
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call917
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore env)
  (save env)
  (save argl)
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (assign val (op lookup-variable-value) (const +) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const +))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch912))
  compiled-branch913
  (assign continue (label after-call914))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch912
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call914
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore env)
  (save env)
  (save argl)
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (assign val (op lookup-variable-value) (const null?) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const null?))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch909))
  compiled-branch910
  (assign continue (label after-call911))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch909
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call911
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore env)
  (save env)
  (save argl)
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (assign val (op lookup-variable-value) (const cons) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const cons))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch906))
  compiled-branch907
  (assign continue (label after-call908))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch906
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call908
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore env)
  (save env)
  (save argl)
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (assign val (op lookup-variable-value) (const cdr) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const cdr))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch903))
  compiled-branch904
  (assign continue (label after-call905))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch903
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call905
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (assign val (op lookup-variable-value) (const car) (reg env))
  (assign argl (op list) (reg val))
  (assign val (const car))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch900))
  compiled-branch901
  (assign continue (label after-call902))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch900
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call902
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch927))
  compiled-branch928
  (assign continue (label after-call929))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch927
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call929
  (restore env)
  (perform (op define-variable!) (const primitive-procedures) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry930) (reg env))
  (goto (label after-lambda931))
  entry930
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const ()) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const map) (reg env))
  (assign val (op lookup-variable-value) (const primitive-procedures) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const car) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch932))
  compiled-branch933
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch932
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call934
  after-lambda931
  (perform (op define-variable!) (const primitive-procedure-names) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry935) (reg env))
  (goto (label after-lambda936))
  entry935
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const ()) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const map) (reg env))
  (assign val (op lookup-variable-value) (const primitive-procedures) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op make-compiled-procedure) (label entry937) (reg env))
  (goto (label after-lambda938))
  entry937
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (proc)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const cadr) (reg env))
  (assign val (op lookup-variable-value) (const proc) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch939))
  compiled-branch940
  (assign continue (label after-call941))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch939
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call941
  (assign argl (op list) (reg val))
  (assign val (const primitive))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch942))
  compiled-branch943
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch942
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call944
  after-lambda938
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch945))
  compiled-branch946
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch945
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call947
  after-lambda936
  (perform (op define-variable!) (const primitive-procedure-objects) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry948) (reg env))
  (goto (label after-lambda949))
  entry948
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (proc args)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const apply-in-underlying-scheme) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const args) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const primitive-implementation) (reg env))
  (assign val (op lookup-variable-value) (const proc) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch950))
  compiled-branch951
  (assign continue (label after-call952))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch950
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call952
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch953))
  compiled-branch954
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch953
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call955
  after-lambda949
  (perform (op define-variable!) (const apply-primitive-procedure) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry956) (reg env))
  (goto (label after-lambda957))
  entry956
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const ()) (reg argl) (reg env))
  (assign proc (op make-compiled-procedure) (label entry958) (reg env))
  (goto (label after-lambda959))
  entry958
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (initial-env)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const define-variable!) (reg env))
  (assign val (op lookup-variable-value) (const initial-env) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const true) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (const true))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch960))
  compiled-branch961
  (assign continue (label after-call962))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch960
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call962
  (restore env)
  (restore continue)
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const define-variable!) (reg env))
  (assign val (op lookup-variable-value) (const initial-env) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const false) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (const false))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch963))
  compiled-branch964
  (assign continue (label after-call965))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch963
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call965
  (restore env)
  (restore continue)
  (assign val (op lookup-variable-value) (const initial-env) (reg env))
  (goto (reg continue))
  after-lambda959
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const extend-environment) (reg env))
  (save proc)
  (assign val (op lookup-variable-value) (const the-empty-environment) (reg env))
  (assign argl (op list) (reg val))
  (save env)
  (save argl)
  (assign proc (op lookup-variable-value) (const primitive-procedure-objects) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch969))
  compiled-branch970
  (assign continue (label after-call971))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch969
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call971
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const primitive-procedure-names) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch966))
  compiled-branch967
  (assign continue (label after-call968))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch966
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call968
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch972))
  compiled-branch973
  (assign continue (label after-call974))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch972
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call974
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch975))
  compiled-branch976
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch975
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call977
  after-lambda957
  (perform (op define-variable!) (const setup-environment) (reg val) (reg env))
  (assign val (const ok))
  (save env)
  (assign proc (op lookup-variable-value) (const setup-environment) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch978))
  compiled-branch979
  (assign continue (label after-call980))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch978
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call980
  (restore env)
  (perform (op define-variable!) (const the-global-environment) (reg val) (reg env))
  (assign val (const ok))
  (assign val (const ";;; M-Eval input:"))
  (perform (op define-variable!) (const input-prompt) (reg val) (reg env))
  (assign val (const ok))
  (assign val (const ";;; M-Eval value:"))
  (perform (op define-variable!) (const output-prompt) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry981) (reg env))
  (goto (label after-lambda982))
  entry981
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const ()) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const prompt-for-input) (reg env))
  (assign val (op lookup-variable-value) (const input-prompt) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch983))
  compiled-branch984
  (assign continue (label after-call985))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch983
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call985
  (restore env)
  (restore continue)
  (save continue)
  (save env)
  (assign proc (op make-compiled-procedure) (label entry986) (reg env))
  (goto (label after-lambda987))
  entry986
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (input)) (reg argl) (reg env))
  (assign proc (op make-compiled-procedure) (label entry988) (reg env))
  (goto (label after-lambda989))
  entry988
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (output)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const announce-output) (reg env))
  (assign val (op lookup-variable-value) (const output-prompt) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch990))
  compiled-branch991
  (assign continue (label after-call992))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch990
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call992
  (restore env)
  (restore continue)
  (assign proc (op lookup-variable-value) (const user-print) (reg env))
  (assign val (op lookup-variable-value) (const output) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch993))
  compiled-branch994
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch993
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call995
  after-lambda989
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const eval) (reg env))
  (assign val (op lookup-variable-value) (const the-global-environment) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const input) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch996))
  compiled-branch997
  (assign continue (label after-call998))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch996
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call998
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch999))
  compiled-branch1000
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch999
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call1001
  after-lambda987
  (save proc)
  (assign proc (op lookup-variable-value) (const read) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1002))
  compiled-branch1003
  (assign continue (label after-call1004))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1002
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call1004
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1005))
  compiled-branch1006
  (assign continue (label after-call1007))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1005
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call1007
  (restore env)
  (restore continue)
  (assign proc (op lookup-variable-value) (const driver-loop) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1008))
  compiled-branch1009
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1008
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call1010
  after-lambda982
  (perform (op define-variable!) (const driver-loop) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry1011) (reg env))
  (goto (label after-lambda1012))
  entry1011
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (string)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const newline) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1013))
  compiled-branch1014
  (assign continue (label after-call1015))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1013
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call1015
  (restore env)
  (restore continue)
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const newline) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1016))
  compiled-branch1017
  (assign continue (label after-call1018))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1016
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call1018
  (restore env)
  (restore continue)
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const display) (reg env))
  (assign val (op lookup-variable-value) (const string) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1019))
  compiled-branch1020
  (assign continue (label after-call1021))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1019
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call1021
  (restore env)
  (restore continue)
  (assign proc (op lookup-variable-value) (const newline) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1022))
  compiled-branch1023
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1022
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call1024
  after-lambda1012
  (perform (op define-variable!) (const prompt-for-input) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry1025) (reg env))
  (goto (label after-lambda1026))
  entry1025
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (string)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const newline) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1027))
  compiled-branch1028
  (assign continue (label after-call1029))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1027
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call1029
  (restore env)
  (restore continue)
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const display) (reg env))
  (assign val (op lookup-variable-value) (const string) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1030))
  compiled-branch1031
  (assign continue (label after-call1032))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1030
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call1032
  (restore env)
  (restore continue)
  (assign proc (op lookup-variable-value) (const newline) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1033))
  compiled-branch1034
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1033
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call1035
  after-lambda1026
  (perform (op define-variable!) (const announce-output) (reg val) (reg env))
  (assign val (const ok))
  (assign val (op make-compiled-procedure) (label entry1036) (reg env))
  (goto (label after-lambda1037))
  entry1036
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (object)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const compound-procedure?) (reg env))
  (assign val (op lookup-variable-value) (const object) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1041))
  compiled-branch1042
  (assign continue (label after-call1043))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1041
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call1043
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch1039))
  true-branch1038
  (assign proc (op lookup-variable-value) (const display) (reg env))
  (save continue)
  (save proc)
  (assign proc (op lookup-variable-value) (const list) (reg env))
  (save proc)
  (assign val (const <procedure-env>))
  (assign argl (op list) (reg val))
  (save env)
  (save argl)
  (assign proc (op lookup-variable-value) (const procedure-body) (reg env))
  (assign val (op lookup-variable-value) (const object) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1047))
  compiled-branch1048
  (assign continue (label after-call1049))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1047
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call1049
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const procedure-parameters) (reg env))
  (assign val (op lookup-variable-value) (const object) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1044))
  compiled-branch1045
  (assign continue (label after-call1046))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1044
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call1046
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (assign val (const compound-procedure))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1050))
  compiled-branch1051
  (assign continue (label after-call1052))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1050
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call1052
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1053))
  compiled-branch1054
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1053
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call1055
  false-branch1039
  (assign proc (op lookup-variable-value) (const display) (reg env))
  (assign val (op lookup-variable-value) (const object) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1056))
  compiled-branch1057
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1056
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call1058
  after-if1040
  after-lambda1037
  (perform (op define-variable!) (const user-print) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const driver-loop) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1059))
  compiled-branch1060
  (assign continue (label after-call1061))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1059
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call1061

      
      )))

;;; Tests

(define the-global-environment (setup-environment))
(set-register-contents! scheme-machine 'env (get-global-environment))
;(scheme-machine 'trace-on)

(start scheme-machine)

(get-register-contents scheme-machine 'val)