;; Just designs for the syntax transformation here. No implementations
;; given.

(while cond body)

;; is:

;; Mind blow time again!
((lambda (f) (f f))
 (lambda (f)
   (if cond (append body (f f))))) ; Isn't there still name clash?

;; For eval case analysis:
((while? exp)
 ;; Don't need env in while->combination since it's just a syntax
 ;; transformation. Will need the outer eval though.
 (eval (while->combination exp) env))

;; until is just (while (not cond) body)

(do ((i 0 (+1 i)) (< i 5)) body)

;; is:

(let ((i 0))
  (while (< i 5)
    (append body (+ 1 i))))

;; Edit: It can be done without gensym!
;; http://gurugio.blogspot.com/2011/02/sicp-exercise-ex49.html
;; Strategy is to establish the loop in the implementor language, not
;; within the implemented language.
(define (eval-while exp env)
  (define while-predicate (lambda (exp) (cadr exp)))
  (define while-body (lambda (exp) (cddr exp)))
  (cond ((true? (eval (while-predicate exp) env)) ; Alternatively, could
         (eval-sequence (while-body exp) env)     ; use while.
         (eval-while exp env))))

;; So it seems like while can't be implemented purely as a syntax
;; transform. We need to add special handling for it within the evaluator.

;;; Test

(define (test)
  (define count 0)
  (define (increment y)
    (if (= count 5)
        false
        (set! count (+ count y))))
  increment)

(define inkr (test))

(while (inkr 1) (+ 2 3))
