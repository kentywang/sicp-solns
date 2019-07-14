;; If the specs for the language are to allow internal definitions to behave as
;; if they were simultaneous defined (or perhaps or formally, as if the evaluation
;; of each definition was at a point in time where all other definitions were
;; already evaluated), then Eva is most correct. However, that may be impossible.
;; Here's an example of the challenge:

(define (f x)
  (define a (+ 1 b))
  (define b (+ 1 a))
  (+ x a b))

;; Here, a and b are mutually dependent values (and not procedures). This is a
;; paradox, so Alyssa's approach is the most sane because it errors out.

;; Edit: Excluding these cases, it is possible to sort dependencies like this.
;; It's called topological sorting.