#lang sicp

;;; 4.32

;; We only need to force elements we want to pass to primitive procedures as
;; arguments and elements that are the output for the REPL loop, so unlike
;; Ch. 3 streams, we can jump to the 10th stream without computing the 9
;; elements in front.

;; Edit: infinite trees can also be useful.

;;; 4.33

((quoted? exp)
 (eval-quotation (text-of-quotation exp))

(define (eval-quotation exp env)
  (cond ((pair? exp) (eval (make-cons exp) env))
        (else exp)))

(define (make-cons exp)
  (if (null? exp)
      nil
      (list 'cons (car exp)
            (make-cons (cdr exp)))))

;; Don't actually need nil, since initial null? and nil are both already compatible
;; each other.

; (define-variable! 'nil nil initial-env)
    
;; Would also need to define in implemented language:

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

;;; 4.34

;; Strategy: Use implementation language list to store type tag for cons.
;; Search through the cons local frame for the bindings for x and y and
;; retrieve the thunked arguments, and return just the expression without
;; evaluating.

;; I initially tried to use
;; (eval (list 'car (list 'quote object)) the-global-environment), but this
;; returned the thunk with expression of x or y.

;; Update: Figured it out, thunked x and y are returned because (m x y) is
;; lazy and thunks the variable arguments before they are looked up. So once
;; we use the implemented car and cdr to get back the thunks, we need to lookup
;; their values, which themselves will be thunks that will need to be unwrapped.

;; Would need to add list-underlying to global.
;; Need to use lambda in cons to defer list-underlying's eager evaluation.
(define (cons x y) (list-underlying 'cons-return (lambda (m) (m x y))))
(define (car z) ((list-ref-underlying z 1) (lambda (p q) p)))
(define (cdr z) ((list-ref-underlying z 1) (lambda (p q) q)))

(define (cons-return? z)
  (and (pair? z)
       (tagged-list? z 'cons-return)))
 
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

(cons 1 (cons 2 3)) ; should print (1 & (cons 2 3))