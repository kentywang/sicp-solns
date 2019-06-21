#lang racket

;; In evaluating (magnitude z), apply-generic is called, where type-tags
;; will become the list of the type for each argument. We only have z as
;; our argument, and z's outermost type tag is 'complex, so type-tags is
;; (complex).

;; apply-generic then looks in the table for an operation with the type of
;; (complex) and throws the error when it doesn't find it.

;; If the complex-number selectors were defined as Alyssa suggested, then the
;; execution would continue as follows: the proc in apply-generic would become
;; the same magnitude procedure that was initially evaluated. It would however
;; be applied on the contents of z, so the 'complex type layer is gone. This is
;; another call to apply-generic, but this time the type-tags is (rectangular).

;; Since we have that operation defined thanks to Ben's
;; (install-rectangular-package), we find the proc then to be the internal
;; magnitude in the rectagular package, and use it to calculate the magnitude.