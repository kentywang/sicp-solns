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

;;; Deps

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

;;; Main

;; Not the most efficient.

(define (md3)
  (define perms
    (flatmap (lambda (a)
               (flatmap (lambda (b) 
                          (flatmap (lambda (c)
                                     (flatmap (lambda (d)
                                                (map (lambda (e)
                                                       (list a b c d e))
                                                     (enumerate-interval 1 5)))
                                              (enumerate-interval 1 5)))
                                   (enumerate-interval 1 5)))
                        (enumerate-interval 1 5)))
             (enumerate-interval 1 5)))
  (define (with-names p)
    (list (list 'baker (list-ref p 0))
          (list 'cooper (list-ref p 1))
          (list 'miller (list-ref p 2))
          (list 'fletcher (list-ref p 3))
          (list 'smith (list-ref p 4))))
  (map with-names
       (filter (lambda (p)
                 (let ((baker (list-ref p 0))
                       (cooper (list-ref p 1))
                       (miller (list-ref p 2))
                       (fletcher (list-ref p 3))
                       (smith (list-ref p 4)))
                   (and (not (= 5 baker))
                        (not (= 1 cooper))
                        (not (= 5 fletcher))
                        (not (= 1 fletcher))
                        (> miller cooper)
                        (not (= (abs (- smith fletcher)) 1))
                        (not (= (abs (- fletcher cooper)) 1))
                        (distinct? p))))
               perms)))