(controller
   (assign continue (label expt-done))
 expt-loop
   (test (op =) (reg n) (const 0))
   (branch (label base-case))
   (save continue)
   (assign continue (label after-expt))
   (assign n (op -) (reg n) (const 1))
   (goto (label expt-loop))
 after-expt
   (assign n (op *) (reg b) (reg n))
   (restore continue)
   (goto (reg continue))
 base-case
   (assign n (const 1))
   (goto (reg continue))
 expt-done)