#lang racket
(require "query-eval.rkt")

;; Because the query looks at all assignments of variables that satisfy
;;the pattern XX, there is a distinction between AB and BA.

;; Even for her query, there is a duplicate result.

;; I don't think it's possible to dedupe because any assertion affects
;; both arguments the same way.

;; Edit: lives-near is commutative since and is commutative. We can use
;; lisp-value and filter on the greater of the two values (in terms of
;; string comparision).

(run-query '(assert!
             (rule (lives-near ?person-1 ?person-2)
                   (and (address ?person-1 
                                 (?town . ?rest-1))
                        (address ?person-2 
                                 (?town . ?rest-2))
                        (not (same ?person-1 ?person-2))))))

(run-query '(lives-near ?person (Hacker Alyssa P)))
(run-query '(lives-near ?a ?b))