#lang sicp

;;; Deps

(define us-coins 
  (list 50 25 10 5 1))

(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))

;;; Main

(define (no-more? items) (= (length items) 0))

(define first-denomination car)

(define except-first-denomination cdr)

;;; Tests

(cc 100 us-coins)
(cc 100 uk-coins)
(cc 100 (list 1 5 10 25 50))

;; Order of the denominations wouldn't affect the amount of coin
;; combinations, since our algorithm still branches through all
;; the possibilities.

;; Edit: no-more? can also be defined as null?. Also, an unordered list
;; is much more inefficient. An example case is (cc 50 (list 1 50)),
;; where, instead of spawning a single dead-end branch in each recursion,
;; it would spawn a subtree of two dead-end branches in each recursion.