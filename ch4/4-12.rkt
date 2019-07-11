;; Returns pair of list with variable at front and list with value at
;; front, or false
(define (find-binding var env)
  (define (scan vars vals)
    (cond ((null? vars)
           (find-binding var (enclosing-environment env)))
          ((eq? var (car vars))
           (cons vars vals))
          (else (scan (cdr vars)
                      (cdr vals)))))
  (if (eq? env the-empty-environment)
      false
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))

(define (lookup-variable-value var env)
  (let ((binding (find-binding var env)))
    (if binding
        (cadr binding)
        (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (let ((binding (find-binding var env)))
    (if binding
        (set-car! (cdr binding) val)
        (error "Unbound variable: SET!" var))))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (binding (find-binding var
                                ;; Only pass 1st frame.
                                (list frame))))
    (if binding
        (set-car! (cdr binding) val)
        (assign-binding-to-frame! var val frame))))
