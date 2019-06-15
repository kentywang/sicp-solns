#lang racket
(require "2-78.rkt")

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

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (add (term-list p1)
              (term-list p2)))
        (error "Polys not in same var: 
              ADD-POLY"
               (list p1 p2))))
  
  ;; 2.88
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))
  
  (define (neg-poly p)
    (make-poly (variable p) (negate (term-list p))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (mul (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: 
              MUL-POLY"
               (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))
  ;; 2.87
  (put '=zero? '(polynomial)
       (lambda (p)
         (=zero? (term-list p))))
  ;; 2.88
  (put 'negate '(polynomial)
       (lambda (p)
         (tag (neg-poly p))))
       
  (put 'negate '(scheme-number) -)
  "Installed polynomial package")

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;; 2.89, 2.90
(define (install-sparse-terms-package)
   ;; representation of terms and term lists
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))
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

  (define (neg-terms trms)
    (if (empty-termlist? trms)
        trms
        (let ((t1 (first-term trms)))
          (adjoin-term (make-term (order t1) (negate (coeff t1)))
                       (neg-terms (rest-terms trms))))))
  
  ;; external interface
  (define (tag t) (attach-tag 'sparse t))
  (put 'add '(sparse sparse)
       (lambda (t1 t2)
         (tag (add-terms t1 t2))))
  (put 'mul '(sparse sparse)
       (lambda (t1 t2)
         (tag (mul-terms t1 t2))))
  (put 'negate '(sparse)
       (lambda (t)
         (tag (neg-terms t))))
  (put '=zero? '(sparse) empty-termlist?)
  (put 'make 'sparse tag)
  "Installed sparse terms package.")

(define (install-dense-terms-package)
  ;; representation of terms and term lists
  (define (add-terms L1 L2)
    ;; (print (list L1 L2))
    ;; (newline)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((len1 (length L1))
                 (len2 (length L2)))
             (cond ((= len1 len2)
                    (map + L1 L2))
                   ((< len1 len2)
                    (adjoin-term (first-term L2)
                                 (add-terms L1
                                            (rest-terms L2))))
                   ((> len1 len2)
                    (adjoin-term (first-term L1)
                                 (add-terms (rest-terms L1)
                                            L2))))))))
  ;; (define (mul-terms L1 L2)
  ;;   (if (empty-termlist? L1)
  ;;       (the-empty-termlist)
  ;;       (add-terms
  ;;        (mul-term-by-all-terms
  ;;         (first-term L1) L2)
  ;;        (mul-terms (rest-terms L1) L2))))

  ;; (define (mul-term-by-all-terms t1 L)
  ;;   (if (empty-termlist? L)
  ;;       (the-empty-termlist)
  ;;       (let ((t2 (first-term L)))
  ;;         (adjoin-term
  ;;          (make-term
  ;;           (+ (order t1) (order t2))
  ;;           (mul (coeff t1) (coeff t2)))
  ;;          (mul-term-by-all-terms
  ;;           t1
  ;;           (rest-terms L))))))

  (define (adjoin-term term term-list)
    (cons term term-list))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list)
    (or (null? term-list)
        (andmap =zero? term-list)))
  (define make-term identity)

  (define (neg-terms trms)
    (map negate trms))

  ;; external interface
  (define (tag t) (attach-tag 'dense t))
  (put 'add '(dense dense)
       (lambda (t1 t2)
         (tag (add-terms t1 t2))))
  ;; (put 'mul '(dense dense)
  ;;      (lambda (t1 t2)
  ;;        (tag (mul-terms t1 t2))))
  (put 'negate '(dense)
       (lambda (t)
         (tag (neg-terms t))))
  (put '=zero? '(dense) empty-termlist?)
  (put 'make 'dense tag)
  "Installed dense terms package.")

(define (make-sparse t) ; Expects list of list of order and coeff
  ((get 'make 'sparse) t))

(define (make-dense t) ; Expects list of coeff
  ((get 'make 'dense) t))

;; Other deps

(define (=zero? y) (apply-generic '=zero? y))

(define (install-zero-package)
  ;; (zero? x) is like (= x 0)
  (put '=zero? '(scheme-number) zero?)
  (put '=zero? '(rational) (lambda (x) (zero? (numer x))))
  (put '=zero? '(complex) (lambda (x) (zero? (magnitude x))))
  "Installed zero package.")

(install-polynomial-package)
(install-zero-package)
(install-sparse-terms-package)
(install-dense-terms-package)

;; New
(define (negate x) (apply-generic 'negate x))

;;; Tests

(define p (make-polynomial 'x (make-sparse '((3 4) (1 2) (0 1))))) ; 4x^3 + 2x + 1
(define q (make-polynomial 'x (make-sparse'())))
(define r (make-polynomial 'y (make-sparse'())))
(define s (make-polynomial 'y (make-sparse (list (list 2 p) '(1 3)))))

;; 2.87
(=zero? p)
(=zero? q)

;; 2.88
(sub p p)
(sub q p)
(sub s s)
(sub r s)

;; 2.89
(define t (make-polynomial 'x (make-dense '(4 0 2 1)))) ; 4x^3 + 2x + 1
(define u (make-polynomial 'x (make-dense '())))
(define v (make-polynomial 'y (make-dense '())))
(define w (make-polynomial 'y (make-dense (list p 3 0))))
(define x (make-polynomial 'x (make-dense '(-3 4)))) ; -3x + 4

(add t x)
(sub t t)
(sub t x)
(sub x t)