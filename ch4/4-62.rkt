#lang racket
(require "query-eval.rkt")

;;; 4.62

(run-query '(assert!
             (rule (last-pair (?x) (?x)))))
(run-query '(assert!
             (rule (last-pair (?a . ?b) (?x))
                   (last-pair ?b (?x)))))

(run-query '(last-pair (3) ?x))
(run-query '(last-pair (1 2 3) ?x))
(run-query '(last-pair (2 ?x) (3)))
;; Wouldn't work since it could be any list ending in a 3
;(run-query '(last-pair ?x (3)))