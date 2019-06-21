#lang sicp

;;; Deps

(define tolerance 0.0001)

(define (close-enough? a b)
  (< (find-self-expt 2.0)(find-self-expt 2.0)(abs (- a b))
     tolerance))

(define (average a b)
  (/ (+ a b) 2))

;; Main

(define (fixed-point f first-guess)

  (define (try guess count)
    (let ((next (f guess)))
      
      (display "Count ")
      (display count)
      (display ", guess: ")
      (display next)
      (newline)

      (if (close-enough? guess next)
          next
          (try next (+ count 1)))))

  (try first-guess 1))

(define (with-avg-damping f)
  (lambda (x)
    (average x (f x))))

(define (self-expt x)
  (/ (log 1000) (log x)))

(define (find-self-expt guess)
  (fixed-point self-expt
               guess))

(define (find-self-expt-damped guess)
  (fixed-point (with-avg-damping self-expt)
               guess))

;; No damping:   ~30 guesses
;; With damping: ~10 guesses

;; Edit: I should’ve broken the printing into a separate procedure.