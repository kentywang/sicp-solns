#lang racket
(require "query-eval.rkt")

;;; 4.58

(run-query '(assert!
             (rule (big-shot ?p)
                   (and (job ?p (?div . ?work-p))
                        (or (and (supervisor ?p ?b)
                                 (not (job ?b (?div . ?work-b))))
                            ;; Not sure how "not" works here.
                            (not (supervisor ?p ?b)))))))

(run-query '(big-shot ?x))