#lang racket
(require "query-eval.rkt")

;;; 4.55

;; 1.
(run-query '(supervisor ?name (Bitdiddle Ben)))
;; 2.
(run-query '(job ?name (accounting . ?position)))
;; 3.
(run-query '(address ?name (Slumerville . ?addr)))

;;; 4.56

;; 1.
(run-query '(and (supervisor ?name (Bitdiddle Ben))
                 (address ?name ?addr)))
;; 2.
(run-query '(and (salary (Bitdiddle Ben) ?bsal)
                 (salary ?person ?psal)
                 (lisp-value > ?bsal ?psal)))
;; 3.
(run-query '(and (supervisor ?person ?super)
                 (job ?super ?job)
                 (not (job ?super (computer . ?pos)))))