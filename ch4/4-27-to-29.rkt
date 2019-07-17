#lang racket

;;; 4.27

;; 1. In global env, count is bound to 0.

;; 2. In global env, id is bound to procedure that extends the global env.

;; 3. In evaluating defining w, we encounter an application expression
;; (id (id 10)), so we force the operator of the expression (eval would’ve
;; been sufficient) to get the procedure, then pass that, the operands directly
;; (without evaling them), and a pointer to the global env to apply, which then
;; creates a new frame in front of the passed env (the global) with the
;; thunk-wrapped operand (id 10) that includes a pointer to the global env
;; (where the argument was initially declared).
;; The body of that proc is evaluated here, meaning count is now incremented
;; to 1, and then the argument ('thunk (id 10) <global-env>) is returned,
;; which is what w binds to in the global env.

;; 4. count returns 1

;; 5. w evals to the thunk, which is forced since actual-value, which is
;; (force-it (eval exp env)), is called on the read input. Thus actual-value
;; is called on (id 10) and the global env, which means evaluating (id 10) in
;; the global env (incrementing count to 2), getting back ('thunk 10 <global>),
;; and forcing that, thus returning 10 for the output.

;; 6. count returns 2

;;; 4.28

;; ((id +) 2 3) would need its operator forced, not just evaled, since eval on the
;; operator would just return ('thunk + <global>).

;;; 4.29

;; Memoization would speed up any program that kept evaluating a state variable.

;; Without memoization:
;; (apply * '((force-it ('thunk (id 10) <glo>)) (force-it ('thunk (id 10) <glo>))))
;; (apply * '(10 10)) [count incremented by 1 twice]
;; 100
;; count would be 2

;; With memoization, since the object bound to x is mutated during the first
;; operand's force-evaluation to ('evaluated-thunk 10), on evaluating the second
;; operand, this value is returned and count doesn't increment again, staying at
;; 1.