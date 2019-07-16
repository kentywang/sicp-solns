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

;; Her version will run the same loop on each new call, whereas the book version
;; will have already run the loop at analysis time. This saves time.

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

;; Running (factorial 30000):
;; Original: 7,228,183
;; New: 6,656,237

;; From this we can estimate each analysis + execution takes 7.2m/30k = 241 time
;; units. And just execution[1] is 6.7m/30k = 222 time units. So analysis takes
;; (241-222)/241 = 7.9% of the time of execution.

;; [1] Not strictly true, since there is one analysis that occurs.

;; As predicted recursive procedures benefit greatly from the analyze-once
;; approach.