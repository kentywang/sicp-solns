#lang racket
(require "query-eval.rkt")

;;; 4.63

(run-query '(assert! (son Adam Cain)))
(run-query '(assert! (son Cain Enoch)))
(run-query '(assert! (son Enoch Irad)))
(run-query '(assert! (son Irad Mehujael)))
(run-query '(assert! (son Mehujael Methushael)))
(run-query '(assert! (son Methushael Lamech)))
(run-query '(assert! (wife Lamech Ada)))
(run-query '(assert! (son Ada Jabal)))
(run-query '(assert! (son Ada Jubal)))

(run-query '(assert! (rule (grandson ?g ?s)
                           (and (son ?f ?s)
                                (son ?g ?f)))))
(run-query '(assert! (rule (son ?m ?s)
                           (and (wife ?m ?w)    ; Order matters! If son assert
                                (son ?w ?s))))) ; is first, then never stops.

(run-query '(grandson Cain ?gs))
(run-query '(son Lamech ?s))
(run-query '(grandson Methushael ?gs))