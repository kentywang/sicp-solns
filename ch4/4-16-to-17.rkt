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
        (rebuild defs rest)
        (if (definition? (car body))
            (iter (cdr body) (cons (car body) defs) rest)
            (iter (cdr body) defs (cons (car body) acc)))))
  (define (rebuild defs rest)
    (make-let (map definition-variable defs)
              (map '*unassigned* defs)
              (append (make-sets defs) rest)))
  (define (make-sets defs acc)
    (if (null? defs)
        acc
        (make-sets (cdr defs)
                   (cons (list 'set!
                               (definition-variable (car defs))
                               (definition-value (car defs)))
                         account))))
    (iter body '() '()))

;; 3.
;; If procedure is called frequently, make procedure. Otherwise,
;; procedure body.
