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
(if (true? (eval (while-predicate exp) env))
    (begin
      (eval (while-body exp) env)
      (eval exp env)))
