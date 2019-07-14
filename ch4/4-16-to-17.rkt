;; 1. Modifying from 4.12:
(define (lookup-variable-value var env)
  (let ((binding (find-binding var env)))
    (if binding
        (if (eq? '*unassigned*
                 (cadr binding))
            (error "Unassigned variable" var)
            (cadr binding))
        (error "Unbound variable" var))))

;; 2.
(define (scan-out-defines body)
  (define (iter body defs rest)
    (if (null? body)
        (rebuild defs (reverse rest)) ; Reverse due to cons body sequence.
        (if (definition? (car body))
            (iter (cdr body)
                  (cons (car body) defs)
                  rest)
            (iter (cdr body)
                  defs
                  (cons (car body) rest)))))
  (define (rebuild defs rest)
    ;; List because body is supposed to be a sequence of expressions.
    ;; Could really use an abstraction layer to deal with this.
    (list (make-let (map definition-variable defs)
                    (map (lambda (_) ''*unassigned*) defs)
                    (append (make-sets defs) rest))))
  (define (make-sets defs)
    (if (null? defs)
        '()
        (cons (list 'set!
                    (definition-variable (car defs))
                    (definition-value (car defs)))
              (make-sets (cdr defs)))))
  (iter body '() '()))

;; We also need to add a stop condition to let->combination since we're
;; transforming lambda procedure bodies into a let expression and let
;; expressions are transformed back into lambda procedures:
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

;;; Tests

(define x (lambda (y) (define (z a) (+ y a)) z))
((x 1) 2) ; 3

;; 3.
;; If procedure is called frequently, make procedure. Otherwise,
;; procedure body.
