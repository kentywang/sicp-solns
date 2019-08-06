;#lang racket
;(require "amb-evaluator.rkt")
#lang sicp

;; Scope: just simple & and/or/not queries on predefined rules and assertions.
;; No indexing either.

;; Strategy: Amb branching on simply-query and on getting all the assertions/
;; rules. Require to filter on disjoin (or) and negate (not). Conjoin (and)
;; uses nested qevals as before. Try-again works by cutting off current path.

;; It's actually quite easy with nondeteministic evaluation. We can work at a
;; higher level than thinking about streams of streams of frames.

;; Subtle differences: Infinite loops on the stream-based approach terminate
;; here! The (married Mickey ?who) returns only calculates and returns one
;; result until we tell the program to try again (rather than trying to collect
;; all the results to return at once), so (somewhat paradoxically) the
;; non-stream implementation is lazier. We essentially delay any (perhaps
;; unintended) recursive rule applications.

;; Troubles: How can we get negate to work?
;; A: When entering negate, subsequent evaluation enters amb-free mode to
;; prevent late branching that doesn't actually backtrack to a different
;; frame that passes the negation filter (I initially tried to set a
;; third branch of the simple-query amb called 'no-more-results as a message
;; to know when to stop, but this still necessitated some sort of signal to
;; process evaluations differently after a negate, since any branching that
;; happens after a negate will also have a 'no-more-results if fails, which
;; isn't where we want to stop our search at. I eventually took this third
;; branch out in favor of booleanized versions of the evaluation procedures
;; that simply check if there are results for a given pattern and frame without
;; invoking any amb branching.

;; How does retrying magically get limited to just valid entries?
;; A: Because require prevents it from progressing, so they never reach the
;; last stage, the output.

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) 
       (an-element-of (cdr items))))

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE"
             exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS"
             exp)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols 
                proc (car exp))
               (map-over-symbols 
                proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '? (string->symbol
                  (substring
                   chars 
                   1 
                   (string-length chars))))
        symbol)))

(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (let ((input (read)))
    (if (eq? input 'try-again)
        (amb)
        (let ((q (query-syntax-process input)))
          (newline)
          (display output-prompt)
          (newline)
          (let ((result (qeval q '())))
;            (display "QEVAL RESULT:")
;            (newline)
;            (display result)
;            (newline)
;            (newline)
            ;(display "QEVAL FILTERED RESULT:")(newline)(display result)(newline)
            (display
             (instantiate q
               result
               (lambda (v f)
                 (contract-question-mark v))))
            (newline))))
    (query-driver-loop)))

(define (qeval query frame . bool)
;  (newline)
;  (display "QEVAL")(if (pair? bool) (display "--BOOL"))(newline)
;  (display "query: ")(display query)(newline)
;  (display "frame: ")(display frame)(newline)
;  (newline)
  (let ((qproc (get (type query) qeval-forms)))
    (if qproc
        (if (pair? bool)
            (qproc (contents query) frame true)
            (qproc (contents query) frame))
        (if (pair? bool)
            (simple-query query frame true)
            (simple-query query frame)))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (get t forms)
  (if (null? forms)
      false
      (if (tagged-list? (car forms) t)
          (cadar forms)
          (get t (cdr forms)))))

;; AMB CHANGE
(define (simple-query query-pattern 
                      frame . bool)
  (if (pair? bool)
      (begin ;(display "SIMPLE QUERY--BOOL")(newline)
             (or (find-assertions-bool query-pattern frame)
                 (apply-rules-bool query-pattern frame)))
      (amb (find-assertions query-pattern frame)
           (apply-rules query-pattern frame))))

;; FETCH-ASSERTIONS => THE-ASSERTIONS (AMB)
(define (find-assertions pattern frame)
  (let ((some-assertion (an-element-of THE-ASSERTIONS)))
    (check-an-assertion some-assertion pattern frame)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (find-assertions-bool pattern frame)
  (let ((res (accumulate (lambda (a b)
                (or a b))
              false
              (map (lambda (assertion)
                     (check-an-assertion-bool assertion pattern frame))
                   THE-ASSERTIONS))))
    ;(display "FIND-ASSERTIONS-BOOL RETURN:")(newline)(display res)(newline)
    res))

(define (check-an-assertion-bool
         assertion query-pat query-frame)
  (let ((match-result
         (pattern-match 
          query-pat assertion query-frame)))
    (not (eq? match-result 'failed))))

;; REQUIRE
(define (check-an-assertion 
         assertion query-pat query-frame)
  (let ((match-result
         (pattern-match 
          query-pat assertion query-frame)))
    (require (not (eq? match-result 'failed)))
    match-result))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) 
         (extend-if-consistent 
          pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match 
          (cdr pat) 
          (cdr dat)
          (pattern-match
           (car pat) (car dat) frame)))
        (else 'failed)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match 
         (binding-value binding) dat frame)
        (extend var dat frame))))

;; FETCH-RULES => THE-RULES (AMB)
(define (apply-rules pattern frame)
  (let ((some-rule (an-element-of THE-RULES)))
;    (newline)
;    (display "CHECKING RULE...")
;    (newline)
;    (display some-rule)
;    (newline)
;    (newline)
    (apply-a-rule some-rule pattern frame)))

(define (apply-rules-bool pattern frame)
  (let ((res (accumulate (lambda (a b)
                           ;(display "acc: ")(display a)(newline)(display b)(newline)
                           (or a b))
              false
              (map (lambda (rule)
                     (apply-a-rule-bool rule pattern frame))
                   THE-RULES))))
  ;(display "APPLY-RULES-BOOL RETURN:")(newline)(display res)(newline)
  res))

(define (apply-a-rule-bool rule
                      query-pattern
                      query-frame)
  (let ((clean-rule 
         (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (not (eq? unify-result 'failed))
          (qeval (rule-body clean-rule)
                 unify-result
                 true)
          false))))

;; REQUIRE
(define (apply-a-rule rule
                      query-pattern
                      query-frame)
  (let ((clean-rule 
         (rename-variables-in rule)))
;    (display "CLEAN-RULE:")
;    (newline)
;    (display clean-rule)
;    (newline)
;    (newline)
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (require (not (eq? unify-result 'failed)))
      (qeval (rule-body clean-rule)
             unify-result))))

(define (rename-variables-in rule)
  (let ((rule-application-id 
         (new-rule-application-id)))
    (define (tree-walk exp)
;      (display "TREE-WALK:")
;      (newline)
;      (display exp)
;      (newline)
;      (newline)
      (cond ((var? exp)
;             (newline)
;             (display "VAR! ")(display exp)
;             (newline)
             (make-new-variable 
              exp 
              rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable 
         var rule-application-id)
  (cons '? (cons rule-application-id
                 (cdr var))))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1)
         (extend-if-possible p1 p2 frame))
        ((var? p2)
         (extend-if-possible 
          p2 
          p1 
          frame))        ; ***
        ((and (pair? p1) 
              (pair? p2))
         (unify-match 
          (cdr p1) 
          (cdr p2)
          (unify-match 
           (car p1)
           (car p2)
           frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                   ; ***
           (let ((binding 
                  (binding-in-frame 
                   val
                   frame)))
             (if binding
                 (unify-match
                  var 
                  (binding-value binding) 
                  frame)
                 (extend var val frame))))
          ((depends-on? val var frame)  ; ***
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let
                 ((b (binding-in-frame 
                      e 
                      frame)))
                  (if b
                      (tree-walk 
                       (binding-value b))
                      false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
     (if (number? (cadr variable))
         (string-append
          (symbol->string (caddr variable))
          "-"
          (number->string (cadr variable)))
         (symbol->string (cadr variable))))))

(define (instantiate 
         exp frame unbound-var-handler)
;  (display "INSTANTIATE")
;  (newline)
;  (display exp)
;  (newline)
;  (display frame)
;  (newline)
;  (newline)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding 
                  (binding-in-frame 
                   exp frame)))
             ;(newline)(display "binding: ")(display binding)(newline)
             (if binding
                 (copy 
                  (binding-value binding))
                 (unbound-var-handler 
                  exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) 
                 (copy (cdr exp))))
          (else exp)))
  (copy exp))

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
;  (display variable) (display frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (conjoin conjuncts frame . bool)
  (if (empty-conjunction? conjuncts)
      frame
      (conjoin (rest-conjuncts conjuncts)
               (if (pair? bool)
                   (qeval 
                    (first-conjunct conjuncts)
                    frame
                    true)
                   (qeval 
                    (first-conjunct conjuncts)
                    frame)))))

(define (disjoin disjuncts frame . bool)
  (if (pair? bool)
      (if (empty-disjunction? disjuncts)
          false
          (or (qeval (first-disjunct disjuncts) 
                     frame
                     true)
               (disjoin (rest-disjuncts disjuncts)
                      frame)))
      (begin
        (require (not (empty-disjunction? disjuncts)))
        (amb (qeval (first-disjunct disjuncts) 
                    frame)
             (disjoin (rest-disjuncts disjuncts)
                      frame)))))

;; This one probably won't work. We need to add empty frame as last choice
;; in qeval.

;; No handling for nested nots either.
(define (negate operands frame)
  ;(display "NEGATE")(newline)(display operands)(newline)(display frame)(newline)(newline)
  (let ((has-result (qeval (negated-query operands)
                           frame
                           true)))
    ;(display "!!!")(newline)(display has-result)(newline)
    (require (not has-result))
    ;(display "???")(newline)(display has-result)(newline)
    frame))

(define (always-true ignore frame . bool)
  frame)

(define qeval-forms
  (list (list 'and conjoin)
        (list 'or disjoin)
        (list 'not negate)
        (list 'always-true always-true)))

(define THE-ASSERTIONS
  '((married Minnie Mickey)

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
                (administration big wheel))))

(define THE-RULES
  (query-syntax-process
   '((rule (married ?x ?y)
           (married ?y ?x))

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
                    (outranked-by ?middle-manager ?boss)))))))

(query-driver-loop)

; (and (job ?person (computer programmer))(address ?person ?where))

; (or (supervisor ?x (Bitdiddle Ben))(supervisor ?x (Hacker Alyssa P)))

; (wheel ?who)

; (outranked-by (Scrooge Eben) ?x)

; (and (supervisor ?x (Bitdiddle Ben))(not (job ?x (computer programmer))))

; (lives-near ?x (Bitdiddle Ben))