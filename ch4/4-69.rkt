#lang racket
(require "query-eval.rkt")

;;; Deps

(run-query '(assert!
             (rule (last-pair (?x) (?x)))))
(run-query '(assert!
             (rule (last-pair (?a . ?b) (?x))
                   (last-pair ?b (?x)))))

(run-query '(assert! (son Adam Cain)))
(run-query '(assert! (son Cain Enoch)))
(run-query '(assert! (son Enoch Irad)))
(run-query '(assert! (son Irad Mehujael)))
(run-query '(assert! (son Mehujael Methushael)))
(run-query '(assert! (son Methushael Lamech)))
(run-query '(assert! (wife Lamech Ada)))
(run-query '(assert! (son Ada Jabal)))
(run-query '(assert! (son Ada Jubal)))


(run-query '(assert! (rule (son ?m ?s)          ; For rules at least:
                           (and (wife ?m ?w)    ; Order matters! If son assert
                                (son ?w ?s))))) ; is first, then never stops.

;;; Main

;; Modded grandson rule to have first item be list.
(run-query '(assert! (rule ((grandson) ?g ?s)
                           (and (son ?f ?s)
                                (son ?g ?f)))))
(run-query '(assert!
             (rule ((great . ?rel) ?a ?c)
                   (and (son ?a ?b)
                        (?rel ?b ?c)
                        ;; Last-pair must be last clause in order
                        ;; for (?relationship Adam Irad) to work.
                        (last-pair ?rel (grandson))))))

(run-query '((great grandson) Adam ?who))
(run-query '((great great great great great grandson) Adam ?who))
(run-query '((great grandson) ?g ?ggs))
(run-query '(?relationship Adam Irad))