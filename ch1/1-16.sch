(define (even? n)
  (= (remainder n 2) 0))
(define (square n)
  (* n n))

(define (expt-iter b n a)
  (cond ((= n 1)
         (* a b))
        ((even? n)
         (expt-iter (square b)
                    (/ n 2)
                    a))
        (else
         (expt-iter b
                    (- n 1)
                    (* a b)))))

(define (fast-expt b n)
  (if (= n 0)
      1
      (expt-iter b n 1)))
