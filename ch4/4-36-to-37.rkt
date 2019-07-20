#lang sicp

;;; 4.36

;; In our sequential computational model, we evaluate amb by choosing the first
;; option given, and backtrack if the path terminates via (amb). With three
;; ambs in sequence, and each amb having infinite choices, if the computation
;; terminates on one branch, the backtracking will pick the next choice in the
;; last fork. But since there are infinite paths at that (and every) fork, if
;; the newly chosen branch terminates too, we will continue trying other paths
;; at the same fork, effectively only trying to change one of the three values
;; to satisfy the Pythagorean condition.

;; Basically, depth-first search + infinite paths at last fork = doom.

;; Strategy is to reverse the order of the lets such that all but the first
;; fork have finite branches to traverse through.

;;; Deps

(define (require p)
  (if (not p) (amb)))

(define (an-integer-between a b)
  (require (<= a b))
  (amb a
       (an-integer-between (+ a 1) b)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;;; Main

(define (a-pythagorean-triple-starting-from x)
  (let ((k (an-integer-starting-from x)))
    (let ((j (an-integer-between 1 k)))
      (let ((i (an-integer-between 1 j)))
        (require (= (+ (* i i) (* j j)) 
                    (* k k)))
        (list i j k)))))

;;; 4.37

;; Ben is right. His algorithm has two forks, so there are (on the order of)
;; n^2 paths to explore, while the original has (on the order of) n^3 paths
;; because of three forks. I think Ben's has n(n+1)/2 paths.

;; Edit: Online solutions mention the requires provide some tree pruning to
;; kill off bad branches earlier.