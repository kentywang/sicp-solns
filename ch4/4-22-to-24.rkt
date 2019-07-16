;;; 4.22

;; Just need to add this to analyze's case analysis.
((let? exp)
 (analyze (let->combination exp)))

;;; 4.23

;; When applying a procedure with a body consisting of just a single expression,
;; the text execution directly returns the the application of the one expression
;; (on the env).

;; Alyssa's version will check the nullity of the CDR of the sequence, and then
;; return the application of the CAR.

;; For a two-expression sequence, the text execution results in the application
;; of a lambda (on the env) that applies the first expression and then the
;; second.

;; Alyssa's version iterates through each analyzed expression (via case
;; analysis) and applies each to the env.

;;; 4.24

;; Change driver loop.
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read))
        (start (runtime)))
    (let ((output
           (eval input
                 the-global-environment)))
      (announce-output output-prompt)
      (display-runtime (- (runtime) start))
      (user-print output)))
  (driver-loop))

;; And add new proc.
(define (display-runtime time)
  (display "Runtime: ")
  (newline)
  (display time)
  (newline))

;; Testing with defining factorial:
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;; Running (factorial 400):
;; Original: 42009
;; New: 3630

;; From this we can estimate each analysis + execution takes 42k/400 = 105 time
;; units. And just execution[1] is 3630/400 = 9 time units. So analysis takes about
;; 96/9 = 11 times longer than execution.

;; [1] Not strictly true, since there is one analysis that occurs.

;; Internal lambdas:
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) 
         true 
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) 
         false 
         (ev? ev? od? (- n 1))))))

;; (f 1000):
;; Original: 52771
;; New: 7101

;; Analysis + execution: 52.77
;; Execution[1]: 7.1
;; Analysis:execution ratio: 6.43

;; As predicted recursive procedures benefit greatly from the analyze-once
;; approach.