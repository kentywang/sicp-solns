#lang sicp

;;; 5.46

;;; Compiled:

;; fib(2) => 17/5
;; fib(3) => 27/8 (+10/+3)
;; fib(4) => 47/11 (+20/+3)
;; fib(5) => 77/14 (+30/+3)
;; fib(6) => 127/17 (+50/+3)

;; s(6) = 77 + 47 + k = 127
;; Thus, k = 3.

;; s(4) = 47 = a * fib(5) + b
;;           = a * 5 + b
;;        47 - 5a = b

;; s(5) = 77 = a * fib(6) + b
;;           = a * 8 + b
;;        77 - 8a = b

;; 47 - 5a = 77 - 8a
;; 3a = 30
;; a = 10

;; 47 - 5(10) = b
;; b = -3

;; s(n) = 10 * fib(n+1) - 3

;;; Specialized machine:

;; fib(2) => 4/2
;; fib(3) => 8/4 (+4/+2)
;; fib(4) => 16/6 (+8/+2)
;; fib(5) => 28/8 (+12/+2)
;; fib(6) => 48/10 (+20/+2)

;; s(6) = 28 + 16 + k = 48
;; Thus, k = 4.

;; s(4) = 16 = a * fib(5) + b
;;           = a * 5 + b
;;        16 - 5a = b

;; s(5) = 28 = a * fib(6) + b
;;           = a * 8 + b
;;        28 - 8a = b

;; 16 - 5a = 28 - 8a
;; 3a = 12
;; a = 4

;; 47 - 5(4) = b
;; b = 27

;; s(n) = 4 * fib(n+1) + 27

;; Specialized machine: 4 * fib(n+1) + 27 for pushes, 2(n-2)+2 for depth
;; Evaluator: 56 * fib(n+1)-40 for pushes, 5(n-2)+13 for depth
;; Compiled: 10 * fib(n+1)-3 for pushes, 3(n-2)+5 for depth

;; Ratio of compiled:interpreted:
;; 10:56 for pushes, 3:5 for depth

;; The depth ratio approaches the constant of 0.6 (for depth) as n becomes
;; large. Pushes are not a function of n, so that ratio doesn't approach a
;; constant independent of n. Though I guess it approaches the constant of
;; ~0.1786 with respect to fib(n+1).

;; That's 5.6x greater for interpreted stack pushes and 1.67x for interpreted
;; stack depth, for what it's worth.

;; Ratio of specialized:interpreted:
;; 4:56 for pushes, 2:5 for depth

;; The ratios should approach the constants of 0.0714 (for pushes wrt fib(n+1))
;; and 0.4 (for depth as n becomes large)

;; That's 14x greater for interpreted stack pushes and 2.5x for interpreted
;; stack depth, for what it's worth.