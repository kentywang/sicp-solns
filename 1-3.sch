(define (square x) (* x x))

(define (sum-of-larger-squares x y z)
  (cond ((and (not (< x y)) (not (< x z))) ; x is largest
         (+ (square x) (if (not (< y z))
                           (square y)
                           (square z))))
        ((and (not (< y x)) (not (< y z))) ; y is largest
         (+ (square y) (if (not (< x z))
                           (square x)
                           (square z))))
        ((and (not (< z x)) (not (< z y))) ; z is largest
         (+ (square z) (if (not (< x y))
                           (square x)
                           (square y))))))

