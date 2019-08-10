(controller
 sqrt-loop
   (assign x (op read)) ; x←rd (didn't include in diagram)
   (assign guess (const 1.0)) ; guess←1.0
 good-enough?
   (assign store (op square) (reg guess)) ; store←square
   (assign store (op -) (reg store) (reg x)) ; store←-
   (assign store (op abs) (reg store)) ; store←abs
   (test (op <) (reg store) (const 0.001))
   (branch (label sqrt-done))
 improve
   (assign store (op /) (reg x) (reg guess)) ; store←/
   (assign guess (op avg) (reg guess) (reg store)) ; guess←avg
   (goto (label good-enough?))
 sqrt-done
   (perform (op print) (reg guess))
   (goto (label sqrt-loop)))