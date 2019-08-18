;; 1. 10 is the maximum stack depth.
;; 2. 1!: 64 pushes
;;    2!: 99 pushes  (+35)
;;    3!: 134 pushes (+35)
;;    4!: 169 pushes (+35)
;;    5!: 204 pushes (+35)
;;    So the formula for number of pushes p for n! where n > 0 is:
;;    p = 35(n - 1) + 64