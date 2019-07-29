#lang racket
(require "query-eval.rkt")

(run-query '(assert!
             (rule (append-to-form () ?y ?y))))
(run-query '(assert!
             (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                   (append-to-form ?v ?y ?z))))

(run-query '(assert!
             (rule (reverse (?a) (?a)))))
;; This rule only works for the (reverse (1 2 3) ?x)) case since the recursive
;; call to reverse will be shrinking down the problem, but in the
;; (reverse ?x (1 2 3)) case, that will be a call to reverse with two unknowns.
(run-query '(assert!
             (rule (reverse (?x . ?y) ?z)
                   (and (reverse ?y ?w)
                        (append-to-form ?w (?x) ?z)))))
;; This rule only works for the (reverse ?x (1 2 3)) case, since it generates
;; a binding for ?z that can be used in the recursive reverse call to shrink the
;; problem. For the (reverse (1 2 3) ?x)) case, the append has infinite
;; solutions.
(run-query '(assert!
             (rule (reverse (?x . ?y) ?z)
                   (and (append-to-form ?w (?x) ?z)
                        (reverse ?y ?w)))))

(run-query '(reverse (1 2 3) ?x))
(run-query '(reverse ?x (1 2 3)))