#lang racket

;; Strategy: Make a frame look like this:

;; ((((? x) (? y) (? z)                 <-- Bindings of the 1st "promise"
;;    some-assertion (? x) (? y) (? z)) <-- 1st not's query
;;   ((? a) (? b)                       <-- Bindings of the 2nd "promise"
;;    some-assertion-2 (? a) (? b)))    <-- 2nd not's query
;;  <#original-frame>)

;; Basically, we have a separate section of the frame data structure for
;; storing promises. When we evaluate a not, we add the promise and
;; immediately try to resolve it. We also try to resolve after each evaluation
;; of a non-special form query.

;; Resolving a promisified frame involves iterating through each promise and
;; if all its variables are bound in the frame, it runs the qeval on the
;; promise's pattern, removing the frame if any qevals return non-empty frames.

(require (rename-in "amb-eval-dep2.rkt"
                    [eval mc-eval]
                    [apply mc-apply])
         "query-eval-dep1.rkt")

(provide run-query
         interpret-query
         query-driver-loop
         make-database)

(define user-initial-environment (setup-environment))

;;;;QUERY SYSTEM FROM SECTION 4.4.4 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm
;;;;Includes:
;;;;  -- supporting code from 4.1, chapter 3, and instructor's manual
;;;;  -- data base from Section 4.4.1 -- see microshaft-data-base below

;;;;This file can be loaded into Scheme as a whole.
;;;;In order to run the query system, the Scheme must support streams.

;;;;NB. PUT's are commented out and no top-level table is set up.
;;;;Instead use initialize-data-base (from manual), supplied in this file.


;;;SECTION 4.4.4.1
;;;The Driver Loop and Instantiation

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (exp->mlist (read)))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           ;; [extra newline at end] (announce-output output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (make-frame '() '()))))
           (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((mpair? exp)
           (mcons (copy (mcar exp)) (copy (mcdr exp))))
          (else exp)))
  (copy exp))


;;;SECTION 4.4.4.2
;;;The Evaluator

(define (qeval query frame-stream)
  ;(display "Qeval: ")(newline)(display query)(newline)(display frame-stream)(newline)(newline)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (stream-flatmap resolve-promises
                        (simple-query query frame-stream)))))

;;;Simple queries

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

;;;Compound queries

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))


(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

;;;Filters

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 4.77

(define (negate-2 operands frame-stream)
  (stream-flatmap resolve-promises
                  (stream-map (lambda (f)
                                (add-promise
                                 (make-promise
                                  (list-vars (negated-query operands))
                                  (negated-query operands))
                                 f))
                              frame-stream)))

(define (add-promise p f)
  ;(display "Add-promise: ")(newline)(display p)(newline)(display f)(newline)(newline)
  (make-frame (mcons p (promises f))
              (bindings f)))

(define (list-vars pattern)
  (define (iter p r)
    (if (null? p)
        r
        (iter (mcdr p)
              (if (var? (mcar p))
                  (mcons (mcar p) r)
                  r))))
  (iter pattern '()))

(define (resolve-promises f)
;  (display "Resolve-promises: ")
;  (newline)
;  (display "Frame promises: ")(display (promises f))(display (mlength (promises f)))
;  (newline)
;  (display "Frame bindings: ")(display (bindings f))
;  (newline)
;  (newline)
  (if (null? (promises f))
      (begin ;(display "SINGLETON")
             (singleton-stream f))
      (let ((first-promise (mcar (promises f))))
        (if (all-vars-bound? first-promise f)
            (if (stream-null? (qeval (promise-pattern first-promise)
                                     (singleton-stream (make-frame (mcdr (promises f)) (bindings f)))))
                (resolve-promises
                 (make-frame (mcdr (promises f)) (bindings f)))
                the-empty-stream)
            (stream-map (lambda (frame)
                          (add-promise first-promise frame))
                        (resolve-promises
                         (make-frame (mcdr (promises f))
                                     (bindings f))))))))

(define (all-vars-bound? p b)
;  (display "All-vars bound? ")
;  (newline)
;  (display "Promise-vars: ")(display (promise-vars p))
;  (newline)
;  (display "Promise-pattern: ")(display (promise-pattern p))
;  (newline)
;  (display "Binding: ")(display b)
;  (newline)
;  (newline)
  (define (iter vs)
    (if (null? vs)
        true
        (and (binding-in-frame (mcar vs) b)
             (iter (mcdr vs)))))
  (iter (promise-vars p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (execute exp)
  (mc-apply (mc-eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream) frame-stream)

;;;SECTION 4.4.4.3
;;;Finding Assertions by Pattern Matching

(define (find-assertions pattern frame)
 (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)

  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (mpair? pat) (mpair? dat))
         (pattern-match (mcdr pat)
                        (mcdr dat)
                        (pattern-match (mcar pat)
                                       (mcar dat)
                                       frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

;;;SECTION 4.4.4.4
;;;Rules and Unification

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((mpair? exp)
             (mcons (tree-walk (mcar exp))
                   (tree-walk (mcdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame)) ; {\em ; ***}
        ((and (mpair? p1) (mpair? p2))
         (unify-match (mcdr p1)
                      (mcdr p2)
                      (unify-match (mcar p1)
                                   (mcar p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                     ; {\em ; ***}
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)    ; {\em ; ***}
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((mpair? e)
           (or (tree-walk (mcar e))
               (tree-walk (mcdr e))))
          (else false)))
  (tree-walk exp))

;;;SECTION 4.4.4.5
;;;Maintaining the Data Base

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (when (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (when (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (mcar pat))
      (var? (mcar pat))))

(define (index-key-of pat)
  (let ((key (mcar pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (mcar pat)))

;;;SECTION 4.4.4.7
;;;Query syntax procedures

(define type type-tag)

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (mcar (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (mcar exps))
(define (rest-conjuncts exps) (mcdr exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (mcar exps))
(define (rest-disjuncts exps) (mcdr exps))

(define (negated-query exps) (mcar exps))

(define (predicate exps) (mcar exps))
(define (args exps) (mcdr exps))


(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (mcadr rule))

(define (rule-body rule)
  (if (null? (mcddr rule))
      ; '(always-true)  ;; ooops
      (mlist 'always-true)
      (mcaddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((mpair? exp)
         (mcons (map-over-symbols proc (mcar exp))
               (map-over-symbols proc (mcdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (mlist '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp)
  (tagged-list? exp '?))

(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (mcons '? (mcons rule-application-id (mcdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
     (if (number? (mcadr variable))
         (string-append (symbol->string (mcaddr variable))
                        "-"
                        (number->string (mcadr variable)))
         (symbol->string (mcadr variable))))))


;;;SECTION 4.4.4.8
;;;Frames and bindings
(define (make-binding variable value)
  (mcons variable value))

(define (binding-variable binding)
  (mcar binding))

(define (binding-value binding)
  (mcdr binding))

(define (binding-in-frame variable frame)
  ;(display "Binding-in-frame:")
  ;(newline)
  ;(display "Var: ")(display variable)(newline)
  ;(display "Frame: ")(display (bindings frame))(newline)
  (if (massoc variable (bindings frame))
      (begin ;(display 'YEP!!!)
             ;(newline)
             ;(newline)
             (massoc variable (bindings frame)))
      (begin ;(display 'NOPE!!!)
             ;(newline)
             ;(newline)
             false)))

(define (extend variable value frame)
  (mcons (promises frame)
         (mcons (make-binding variable value) (bindings frame))))

;;; 4.77

(define (promises f)
  (if (null? f)
      '()
      (mcar f)))
(define (bindings f)
  (if (null? f)
      '()
      (mcdr f)))

(define (make-promise vars pattern)
  (mcons vars pattern))
(define (promise-vars promise)
  (if (null? promise)
      '()
      (mcar promise)))
(define (promise-pattern promise)
  (if (null? promise)
      '()
      (mcdr promise)))

(define (make-frame promises bindings)
  (mcons promises bindings))

;;;;From Section 4.1

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))


;;;; From instructor's manual

(define get '())

(define put '())

(define (initialize-data-base rules-and-assertions)
  (define (deal-out r-and-a rules assertions)
    (cond ((null? r-and-a)
           (set! THE-ASSERTIONS (mlist->stream assertions))
           (set! THE-RULES (mlist->stream rules))
           (void))
          (else
           (let ((s (query-syntax-process (mcar r-and-a))))
             (cond ((rule? s)
                    (store-rule-in-index s)
                    (deal-out (mcdr r-and-a)
                              (mcons s rules)
                              assertions))
                   (else
                    (store-assertion-in-index s)
                    (deal-out (mcdr r-and-a)
                              rules
                              (mcons s assertions))))))))
  (let ((operation-table (make-table)))
    (set! get (operation-table 'lookup-proc))
    (set! put (operation-table 'insert-proc!)))
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'nat 'qeval negate-2)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true)
  (deal-out rules-and-assertions '() '()))

(define microshaft-data-base
  '(
;; from section 4.4.1
(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 60000)

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(salary (Tweakit Lem E) 25000)
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))

(supervisor (Bitdiddle Ben) (Warbucks Oliver))

(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)

(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))

(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(job (Aull DeWitt) (administration secretary))
(salary (Aull DeWitt) 25000)
(supervisor (Aull DeWitt) (Warbucks Oliver))

(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))

(can-do-job (computer programmer)
            (computer programmer trainee))

(can-do-job (administration secretary)
            (administration big wheel))

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?x ?x))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))
))

;; Do following to reinit the data base from microshaft-data-base
;;  in Scheme (not in the query driver loop)
(define (setup-data-base)
  (initialize-data-base (exp->mlist microshaft-data-base)))

(define (make-database rules-and-assertions)
  (initialize-data-base (exp->mlist rules-and-assertions)))

(define (interpret-query query)
  (let ((q (query-syntax-process query)))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base."))
          (else
           (newline)
           ; (display-stream
           (stream->list
            (stream-map
             (lambda (frame)
               (mlist->exp (instantiate q
                             frame
                             (lambda (v f)
                               (contract-question-mark v)))))
             (qeval q (singleton-stream (make-frame '() '())))))))))

(define (run-query exp)
  (interpret-query (exp->mlist exp)))

(setup-data-base)

;;; Tests

(run-query '(and (supervisor ?x (Bitdiddle Ben))
                 (not (job ?x (computer programmer)))))

(run-query '(and (supervisor ?x (Bitdiddle Ben))
                 (nat (job ?x (computer programmer)))))

(run-query '(and (nat (job ?x (computer programmer)))
                 (supervisor ?x (Bitdiddle Ben))))

(run-query '(and (nat (job ?x (computer technician)))
                 (nat (salary ?x 40000)) ; Filters out Alicia.
                 (supervisor ?x (Bitdiddle Ben))))