#lang sicp

;;; Forcing LTR

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((item
             (eval (first-operand exps) env)))
        (cons item
              (list-of-values
               (rest-operands exps)
               env)))))

;;; Forcing RTL

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((item
             (list-of-values
              (rest-operands exps)
              env)))
        (cons (eval (first-operand exps) env)
              item))))

;; Edit: Online answers use nested lets, but my approach should be
;; sufficient.
