#lang sicp

;;; Deps

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;; Register machine

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
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) 
             (initialize))
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
        (inst-by-type '())
        (goto-regs '())
        (saved-or-restored-regs '())
        (assignment-sources '()))
    (let ((the-ops
           (list 
            (list 'initialize-stack
                  (lambda () 
                    (stack 'initialize)))))
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
              (begin
                ((instruction-execution-proc 
                  (car insts)))
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
                 (set! the-instruction-sequence seq)
                 (set! inst-by-type (dedupe-and-sort seq))
                 (set! goto-regs (goto-reg-refs seq))
                 (set! saved-or-restored-regs (find-saved-or-restored-regs seq))
                 (set! assignment-sources (find-assignment-sources seq))))
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
              ((eq? message 'inst-by-type)
               inst-by-type)
              ((eq? message 'goto-regs)
               goto-regs)
              ((eq? message 'saved-or-restored-regs)
               saved-or-restored-regs)
              ((eq? message 'assignment-sources)
               assignment-sources)
              (else (error "Unknown request: 
                            MACHINE"
                           message))))
      dispatch)))

(define (dedupe-and-sort seq)
  (map (lambda (inst)
         ;; Deduping and sorting goes here.
;         (display (car inst))
;         (newline)
;         (newline)
         (instruction-text inst))
       seq))

(define (goto-reg-refs seq)
  (map (lambda (inst)
         (register-exp-reg (instruction-text inst)))
       (filter (lambda (inst)
                 ;; Deduping still needed.
                 (and (eq? (car (instruction-text inst)) 'goto)
                      (register-exp? (register-exp-reg (instruction-text inst)))))
               seq)))

(define (find-saved-or-restored-regs seq)
  (map (lambda (inst)
         (stack-inst-reg-name (instruction-text inst)))
       (filter (lambda (inst)
                 ;; Deduping still needed.
                 (or (eq? (car (instruction-text inst)) 'save)
                     (eq? (car (instruction-text inst)) 'restore)))
               seq)))

(define (find-assignment-sources seq)
  (map (lambda (inst)
         (cons (assign-reg-name (instruction-text inst))
               (assign-value-exp (instruction-text inst))))
       (filter (lambda (inst)
                 ;; Deduping still needed.
                 (eq? (car (instruction-text inst)) 'assign))
               seq)))

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

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text receive)
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
                          next-inst)
                         insts)
                   labels)))))))

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

(define (make-instruction text)
  (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc!
         inst
         proc)
  (set-cdr! inst proc))

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

;;; Tests

(define expt-machine
  (make-machine
    '(n val continue)
    (list (list '- -)
          (list '+ +)
          (list '< <))
    '((assign continue (label fib-done))
      fib-loop
        (test (op <) (reg n) (const 2))
        (branch (label immediate-answer))
        ;; set up to compute Fib(n - 1)
        (save continue)
        (assign continue (label afterfib-n-1))
        (save n)           ; save old value of n
        (assign n 
                (op -)
                (reg n)
                (const 1)) ; clobber n to n-1
        (goto 
         (label fib-loop)) ; perform recursive call
      afterfib-n-1 ; upon return, val contains Fib(n - 1)
        (restore n)
        ;; set up to compute Fib(n - 2)
        (assign n (op -) (reg n) (const 2))
        (assign continue (label afterfib-n-2))
        (save val)         ; save Fib(n - 1)
        (goto (label fib-loop))
      afterfib-n-2 ; upon return, val contains Fib(n - 2)
        (restore n)      ; 5.11: Should fail now!
        (restore continue)
        (assign val        ; Fib(n - 1) + Fib(n - 2)
                (op +) 
                (reg val)
                (reg n))
        (goto              ; return to caller,
         (reg continue))   ; answer is in val
      immediate-answer
        (assign val 
                (reg n))   ; base case: Fib(n) = n
        (goto (reg continue))
      fib-done)))

(set-register-contents! expt-machine 'n 10)

(start expt-machine)

(get-register-contents expt-machine 'val)

(expt-machine 'inst-by-type)
(expt-machine 'goto-regs)
(expt-machine 'saved-or-restored-regs)
(expt-machine 'assignment-sources)