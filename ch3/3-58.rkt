#lang sicp

;; 3.58
;; (expand 1 7 10) produces 1, (expand 3 7 10)
;; (expand 3 7 10) produces 4, (expand 2 7 10)
;; (expand 2 7 10) produces 2, (expand 6 7 10)
;; (expand 6 7 10) produces 8, (expand 4 7 10)
;; (expand 4 7 10) produces 5, (expand 5 7 10)
;; (expand 5 7 10) produces 7, (expand 1 7 10)
;; and then loops again through 1, 4, and 2, 8, 5, 7, ad infinitum.

;; (expand 3 8 10) produces 3, (expand 6 8 10)
;; (expand 6 8 10) produces 7, (expand 4 8 10)
;; (expand 4 8 10) produces 5, (expand 0 8 10)
;; (expand 0 8 10) produces 0, (expand 0 8 10)
;; and loops 0 ad infinitum.

;; It's the quotient of the division by the second argument on an infinitetly
;; expandable number with front digit being the first argument. The third
;; argument is the base.