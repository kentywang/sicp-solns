;;;4.2

;; 1. As the assignment clause is run first, its condition of pair? is
;; fulfilled by anything but a symbol, string, or number, so (define x 2)
;; would be misinterpreted as an application of a procedure. I believe it
;; would thus call (eval define env) and then land in the variable? clause,
;; so it would try to lookup define’s value and find nothing (and then
;; error?).

;; The other possibility is that if the expression passed to the
;; interpreter was a quote, then it would be (eval ‘define env), which
;; would land in eval’s quoted? clause, which would then pass through as
;; define. It would then call (list-of-values ‘(x 2)), do (eval ‘x env)
;; and pass that through as x, then do (eval 2 env) and get 2. It would
;; then call (apply define ‘(x 2)) and, not being being able to identify it
;; as a primitive or compound procedure, would error out.  

;; I don’t think it will be the second option, but I’m not sure how the
;; first option would be implemented considering any arguments passed to
;; eval would be evaluated first by Scheme’s applicative order evaluation.

;;; 2.

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

;; Edit: Consensus online says it's the second option in that it does
;; (eval 'define env), Actually, I do think it's the second.
