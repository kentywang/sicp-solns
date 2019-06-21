;;; Deps

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

;;; Main

(define (estimate-integral p x1 x2 y1 y2 t)
  (define (experiment-in-range)
    (p (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (* (exact->inexact (monte-carlo t experiment-in-range))
     (area x1 x2 y1 y2)))

(define (area a b m n)
  (* (- b a) (- n m))) ; Assumes b > a, n > m.

(define (unit-circle-a t) ; Also is pi.
  (estimate-integral
   (lambda (x y)
     (>= 1 (+ (expt x 2) (expt y 2))))
   -1
   1
   -1
   1
   t))
