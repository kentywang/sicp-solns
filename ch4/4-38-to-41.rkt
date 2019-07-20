#lang sicp

;;; Deps

(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

;;; 4.38

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher 
                      miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
;    (require
;     (not (= (abs (- smith fletcher)) 1)))
    (require 
     (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; Calling (multiple-dewlling) and then (amb) repeatedly generates 5 solutions
;; before the amb tree is exhausted.

;;; 4.39

;; The order of restrictions in this problem does not affect the answer produced,
;; nor does it affect the time to an answer. Our amb's evaluation strategy is to
;; backtrack to the last fork and try the next choice in the sequence, so regardless
;; if conditions were reordered, the set of correct answers remains the same along
;; with the route the amb takes to get there.

;; It might make it a difference in a problem where the restrictions are interleaved
;; with the ambs, though.

;; Edit: The time is affected by the order of restrictions, since distinct? runs
;; in polynomial time while the others are constant. If the distinct? check is
;; done last, that minimizes the set of permutations it needs to check.

;; Notice that we're talking about the runtime of the conditions themselves, not of
;; the number of paths traversed for the ambs to reach an answer, which should
;; remain the same regardless of restriction order.

;; Interesting post from xdavidliu:
;; http://community.schemewiki.org/?sicp-ex-4.39

;;; 4.40

;; Before the distinct, there are 5^5 = 3125 different assignments of people to floors.
;; After the distinct, there are 5! = 120.

;; Strategy is to gradually introduce new ambs after we cull the existing ambs as
;; much as we can. The order of ambs introduced should be such that the choice
;; with fewers paths forward is first.

;; Dependency graph: B M-C-F-S

;; I think that means Baker is first, since by introducing 5 choices (but immediately
;; culling 1 of them[1]) results in fewer paths than introducing Miller-Cooper or Smith-
;; Fletcher, since that would require introducing one amb after another (we need to
;; introduce both at the same time since those requirements look at two entities).

;; [1] This part should be irrelevant for this comparison?

(define (md2)
  (let ((baker (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (let ((miller (amb 1 2 3 4 5)))
        (require (> miller cooper))
        (let ((fletcher (amb 1 2 3 4 5)))
          (require (not (= fletcher 5)))
          (require (not (= fletcher 1)))
          (require 
            (not (= (abs (- fletcher cooper)) 1)))
          (let ((smith (amb 1 2 3 4 5)))
            (require
            (not (= (abs (- smith fletcher)) 1)))
            (require
              (distinct? (list baker cooper fletcher 
                               miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))

;;; 4.41

