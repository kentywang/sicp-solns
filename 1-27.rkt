#lang sicp

;;; Dependencies

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

;;; Main procedure

(define (passes-fermat? n)
  (define (iter a)
    (cond ((= a 1)              ; We’ve evaluated all a < n.
           #t)
          ((= (expmod a n n) a)
           (iter (- a 1))) 
          (else
           #f)))
  (iter (- n 1)))

;; > (passes-fermat? 561)
;; #t

;; > (passes-fermat? 1105)
;; #t

;; > (passes-fermat? 1729)
;; #t

;; > (passes-fermat? 2465)
;; #t

;; > (passes-fermat? 2821)
;; #t

;; > (passes-fermat? 6601)
;; #t
