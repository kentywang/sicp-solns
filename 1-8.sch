(define (improve guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))