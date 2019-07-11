;; Only need to change to constructor, selectors, and mutator?

(define (make-frame variables values)
  (map cons variables values))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (set! frame (cons (cons var val)
                    frame)))

;; Not quite. Still need to modify all the mutators to function without
;; the modified selectors, since they use map and therefore get copied
;; instead of modified.

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (let ((binding (assoc var frame)))
        (if binding
            (set-cdr! binding val)
            (env-loop
             (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (scan (first-frame env))))
  (env-loop env))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (binding (assoc var frame)))
    (if binding
        (set-cdr! binding val)
        (add-binding-to-frame!
         var val frame))))
