#lang sicp

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((small (up-split painter (dec n))))
        (below painter
               (beside small small)))))
