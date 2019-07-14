#lang sicp

;;; 1.

;; We're looking to transform:
(letrec ((a 0)
         (b 1))
  (+ a b))

;; into:
(let ((a '*unassigned)
      (b '*unassigned))
  (set! a 0)
  (set! b 1)
  (+ a b))

;; For simplicity (but certainly not for an actual implementation), we could
;; append to the body of the letrec definitions for the letrec's variables, and
;; pass the result into scan-out-defines, which we already built for internal
;; procedures.

;; For eval
((letrec? exp)                       ; LETREC
 (eval (scan-out-defines
        (add-defs-to-body
         (let-vars exp)
         (let-exps exp)
         (let-body exp)))
       env))

;; New
(define (add-defs-to-body vars exps body)
  (if (null? vars)
      body
      (cons (list 'define (car vars) (car exps))
            (add-defs-to-body (cdr vars) (cdr exps) body))))

;; I had to modify the original definition of scan-out-defines to not wrap
;; the output in a list, so that we can use it for let-rec without unwrapping.
;; Could have done the latter too, my choice.