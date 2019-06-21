(define (even? x) (= (mod x 2) 0))
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (* a b)
  (define (iter a b sum)
    (cond ((= b 0)
           sum)
          ((even? b)
           (iter (double a) (halve b) sum))
          (else
           ; This should have fewer calls:
           ; (iter (double a) (halve (- b 1 )) (+ sum a)))))
           (iter a (- b 1) (+ sum a)))))
  (iter a b 0))
