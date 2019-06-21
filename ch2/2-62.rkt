#lang sicp

;;; Main

(define (union-set s t)
  (cond ((null? s) t)
        ((null? t) s)
        ;; Head of s not in t (and smaller than anything in t), so add it to
        ;; the head of the new list.
        ((< (car s) (car t)) (cons (car s)
                                   (union-set (cdr s) t)))
        ((= (car s) (car t)) (cons (car s)
                                   (union-set (cdr s) (cdr t))))
        ;; Inverse of other condition.
        (else (cons (car t)
                    (union-set s (cdr t))))))

;;; Tests
   
(union-set '(1 2 3) '())
(union-set '(1 3 5 7) '(2 4 6 8))
(union-set '(1 3 5 7) '(2 5 7))
(union-set '(6 7 8) '(2 9 10))