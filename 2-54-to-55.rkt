#lang sicp

;;; 2.54

(define (equal? a b)
   (or (eq? a b)
       (and (pair? a)
            (pair? b)
            (equal? (car a) (car b))
            (equal? (cdr a) (cdr b)))))

;;; Tests

(equal? '(this is a list) 
        '(this is a list))

(equal? '(this is a list) 
        '(this (is a) list))

(equal? '(this () list) 
        '(this () list))

;;; 2.55

(car ''abracadabra)

;; Same as:
(car '(quote abracadabra))

;; This is a list with quote as the first element.