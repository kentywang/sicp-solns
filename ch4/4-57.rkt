#lang racket
(require "query-eval.rkt")

;;; 4.57

(run-query '(assert!
             (rule (replaceable-by ?person-2 ?person-1)
                   (and (or (and (job ?person-1 ?job)
                                 (job ?person-2 ?job))
                            (and (job ?person-1 ?job-1)
                                 (job ?person-2 ?job-2)
                                 (can-do-job ?job-1 ?job-2)))
                        (not (same ?person-1 ?person-2))))))

;; 1.
(run-query '(replaceable-by (Fect Cy D) ?person))
;; 2.
(run-query '(and (replaceable-by ?higher-paid ?lower-paid)
                 (salary ?higher-paid ?higher-salary)
                 (salary ?lower-paid ?lower-salary)
                 (lisp-value < ?lower-salary ?higher-salary)))