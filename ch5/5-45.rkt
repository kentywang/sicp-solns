#lang sicp

;;; 5.45

;;; 1. Recursive factorial:

;; f(1) => 7/3
;; f(2) => 13/5 (+6/+2)
;; f(3) => 19/8 (+6/+3)
;; f(4) => 25/11 (+6/+3)

;; Specialized machine: 2(n-1) for both pushes and depth
;; Evaluator: 32(n-1)+16 for pushes, 5(n-1)+8 for depth
;; Compiled: 6(n-1)+7 for pushes, ~3(n-1)+3 for depth

;; Ratio of compiled:interpreted (using just factor of n):
;; 6:32 for pushes, 3:5 for depth

;; The ratios should approach the constants of 0.1875 (for pushes)
;; and 0.6 (for depth) as n becomes large.

;; That means it's 5.33x slower and needs 1.67x more space for the interpreted.

;; Ratio of specialized:interpreted (using just factor of n):
;; 2:32 for pushes, 2:5 for depth

;; The ratios should approach the constants of 0.0625 (for pushes)
;; and 0.4 (for depth) as n becomes large.

;; That means it's 16x slower and needs 2.5x more space for the interpreted.

;; Thus, it's 16/5.33 = 3x faster for the program to run on a specialized
;; machine and uses 2.5/1.67 = 1.5x less memory.

;;; 2.
;; The memory usage difference is smaller than the time usage difference,
;; so I'd focus on eliminating as many instructions to the stack as possible.

;; The compiled version has saves env, continue (twice), argl, and proc (twice), whereas
;; the specialized version just saves continue and n once.

;; The first push/restore of continue and env is on applying (= n 1), so if we can
;; know ahead of time that = is not a compound procedure, we could avoid using the
;; stack there. We'd need to know what all the primitive procedures were, at the
;; start.

;; Changing the order of the compilation so that the operand is evaluated last
;; eliminates two more save/restores, these on proc. If we can evaluate the n
;; operand after the (factorial (- n 1)) operand, we can also eliminate another
;; save/restore, this one on argl (we could determine which order of operand
;; evaluation results in the fewest stack usage during compile time).

;; Actually, since argl in this case is like the n in the specialized version,
;; it definitely needs to be save/restored, so we can't eliminate that.

;; It doesn't operate on the stack, but there's also a lot of lookup of that
;; might be able to be trimmed too.