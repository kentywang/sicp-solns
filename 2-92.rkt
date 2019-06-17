#lang racket
(require "2-92d.rkt")

;; New deps

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))
             (print (list "first term:" t1 "second term:" t2))
             (newline)
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 
                     (add-terms (rest-terms L1) 
                                L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 
                     (add-terms 
                      L1 
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term 
                      (order t1)
                      (add (coeff t1) 
                           (coeff t2)))
                     (add-terms 
                      (rest-terms L1)
                      (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms 
         (mul-term-by-all-terms 
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term 
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms 
            t1 
            (rest-terms L))))))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) 
    (null? term-list))
  (define (make-term order coeff) 
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; 2.92
  (define (coerce-to var poly)
    (define (swap-terms p)
      (let ((var (variable p))
            (ts (term-list p)))
        (map (lambda (t)
                   ;(if (eq? (type-tag t)
                   ;         'polynomial)
                   ;  ()
                   ;; Term not polynomial, so order 0.
                   ;; TODO: Need to unwrap type tag.
                   (make-term 0 (make-polynomial var (list t))))
                 ts)))
    (make-poly var (swap-terms poly))) ; Not sorted. Or merged.

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        ;; 2.92
        (make-poly 
         (variable p1)
         (add-terms (term-list p1)
                    (term-list (coerce-to (variable p1) p2))))))
 
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (mul-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: 
              MUL-POLY"
               (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))
  ;; 2.87
  (put '=zero? '(polynomial)
       (lambda (n)
         (empty-termlist? (term-list n))))
  ;; 2.92
  (put 'add '(scheme-number polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  "Installed polynomial package")

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-polynomial-package)

;;; Tests

(define p (make-polynomial 'x '((3 4) (1 2) (0 1)))) ; 4x^3 + 2x + 1
(define q (make-polynomial 'x '()))
(define r (make-polynomial 'y '()))
(define s (make-polynomial 'y (list (list 2 p) '(1 3))))
(define t (make-polynomial 'y '((2 5) (1 3) (0 4)))) ; 5y^2 + 3y + 4
(define u (make-polynomial 'y `((1 ,p))))

;; 2.92
(add p t)
;(add p u)