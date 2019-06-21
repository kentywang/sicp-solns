;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1

(define (pascal depth i)
  (if (or (= i 0) (= i depth))
      1
      (+ (pascal (- depth 1) i)
         (pascal (- depth 1) (- i 1))))