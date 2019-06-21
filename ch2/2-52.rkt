#lang scheme

;; 1.
(define right (make-segment (make-vect 0.7 0.5) (make-vect 1.0 1.0)))
(define mid (make-segment (make-vect 0.3 0.5) (make-vect 0.7 0.5)))
(define left (make-segment (make-vect 0.0 1.0) (make-vect 0.3 0.5)))
(segment-painter (list right mid left))

;; 2.
(definite (corner-split p n)
  (if (= n 0)
      p
      (below (beside p p)
             (beside p (corner-split p (- n 1))))))

;; 3. Already how it is.

;; Edit: Mistake on 2. We still want split on the top and right corners, but
;; starting as one image instead of two. So we eliminate the beside and below
;; for each.