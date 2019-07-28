#lang racket
(require "query-eval.rkt")

;;; Example logic program

(run-query '(assert!
             (rule (append-to-form () ?y ?y))))
(run-query '(assert!
             (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                   (append-to-form ?v ?y ?z))))

(run-query '(append-to-form (a b) ?x (a b c d)))

;;; 4.61

(run-query '(assert!
             (rule (?x next-to ?y in (?x ?y . ?u)))))
(run-query '(assert!
             (rule (?x next-to ?y in (?v . ?z))
                   (?x next-to ?y in ?z))))

(run-query '(?x next-to ?y in (1 (2 3) 4)))
;; Should return
;;((1 next-to (2 3) in (1 (2 3) 4))
;; ((2 3) next-to 4 in (1 (2 3) 4)))

(run-query '(?x next-to 1 in (2 1 3 1)))
;; Should return
;; ((2 next-to 1 in (2 1 3 1)))
;;  (3 next-to 1 in (2 1 3 1))))