
;; 4.25

;; In applicative-order, (factorial 5) will not end, since it will keep evaluating
;; new recursive calls to pass into unless.

;; In normal-order, it will work. It might return (* 5 4 3 2 1) unless we specify
;; that user-output is to be evaluated.

;; 4.26

;; Similar to eval-if, but with consequent and alternative swapped.
(define (unless->if exp)
  (make-if (unless-predicate exp)
           (unless-alternative exp)
           (unless-consequent exp)))

;; If we wanted to map over a set of conditions, it would be useful to have
;; unless be a procedure instead of a special form.