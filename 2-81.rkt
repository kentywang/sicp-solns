#lang sicp

;; 1. We are stuck in a recursive loop as we continually coerce one type to
;; another and then run apply-generic on the coerced first datum and the
;; original second datum.

;; 2. It worked fine as-is, since if a proc was not found between two things
;; off the same type, coercion would not help.

;;3.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args))) 
                (if (not (equal? type1 type2))
                    (let ((t1->t2 
                           (get-coercion type1
                                         type2))
                          (t2->t1 
                           (get-coercion type2 
                                         type1)))
                      (cond (t1->t2
                             (apply-generic 
                              op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic 
                              op a1 (t2->t1 a2)))
                            (else
                             (error 
                              "No method for 
                           these types"
                              (list 
                               op 
                               type-tags)))))
                    (error 
                     "No method for these types"
                     (list op type-tags))))
              (error 
               "No method for these types"
               (list op type-tags)))))))


