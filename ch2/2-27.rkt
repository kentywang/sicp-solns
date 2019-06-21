#lang sicp

(define (deep-reverse items)
  (define (iter things acc)
      (if (null? things)
          acc
          (let ((curr (car things)))
            (iter (cdr things)
                  (cons (if (pair? curr)
                            (deep-reverse curr)
                            curr)
                        acc)))))
  (iter items nil))

(define x 
  (list (list 1 2) (list 3 4)))

(deep-reverse x)

;; Edit: Correct solution using map, which was what the book is going for:
(define (deep-reverse2 tree)
  (if (pair? tree)
      (reverse (map deep-reverse2 tree))
      tree))