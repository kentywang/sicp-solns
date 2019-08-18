;; 5.26: Iterative

;; 1. 10 is the maximum stack depth.∎

;; 2. 1!: 64 pushes
;;    2!: 99 pushes  (+35)
;;    3!: 134 pushes (+35)
;;    4!: 169 pushes (+35)
;;    5!: 204 pushes (+35)

;;    So the formula for number of pushes p for n! where n > 0 is:
;;    p = 35(n - 1) + 64 ∎

;; 5.27: Recursive

;; 1!: 16p, 8sd (aka 16/8)
;; 2!: 48/13 (+32/+5)
;; 3!: 80/18 (+32/+5)
;; 4!: 112/23 (+32/+5)

;; So the formula for number of pushes p for n! where n > 0 is:
;; p = 32(n - 1) + 16 ∎

;; And the stack depth formula is:
;; d = 5(n - 1) + 8 ∎

;; The book says max depth relates to space complexity, while no. of pushes
;; relates to time complexity. Thus we have demonstrated that the iterative
;; process has constant space, linear time, while the recursive process has
;; linear space (from the stack buildup) and linear time.