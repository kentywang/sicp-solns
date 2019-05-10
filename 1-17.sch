;; 6 * 5 = 12 * 2 + 6 = 24 * 1 + 6

(define (multiply a b)
  (cond ((= b 0)
         0)
        ((even? b)
         (multiply (double a)
                   (halve b)))
        (else (+ a
                 (multiply a
                           (- b 1))))))
