#lang racket
(require "query-eval.rkt")

;;; 4.59

(run-query '(assert! (meeting accounting (Monday 9am))))
(run-query '(assert! (meeting administration (Monday 10am))))
(run-query '(assert! (meeting computer (Wednesday 3pm))))
(run-query '(assert! (meeting administration (Friday 1pm))))
(run-query '(assert! (meeting whole-company (Wednesday 4pm))))

;; 1.
(run-query '(meeting ?div (Friday ?time)))
;; 2.
(run-query '(assert!
             (rule (meeting-time ?person ?day-and-time)
                   (or (meeting whole-company ?day-and-time)
                       (and (job ?person (?div . ?work))
                            (meeting ?div ?day-and-time))))))
;; 3.
(run-query '(meeting-time (Hacker Alyssa P) (Wednesday ?t)))