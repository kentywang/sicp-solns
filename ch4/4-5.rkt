#lang sicp

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
  (eq? (cdr clause) '=>))
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
                     ;; New
                     (if (cond-special-clause? first)
                         ;; We could cache the predicate, but book hasn't
                         ;; introduced make-let yet.
                         (list (cond-special-proc first)
                               (cons-predicate first))
                         ;; Careful, cond-actions returns => for
                         ;; the special clause, which is wrong.
                         (sequence->exp
                          (cond-actions first)))
                     (expand-clauses rest))))))

;; Edit: Originally, I had the consequent being
;; ((cond-special-proc first) (cons-predicate first)), but it seems I
;; shouldn't be evaluating. This is just to transform the syntax to be
;; evaluated further on.
