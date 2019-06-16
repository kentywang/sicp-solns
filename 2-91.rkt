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

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist)
              (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1)
                                (coeff t2)))
                    (new-o (- (order t1)
                              (order t2))))
                (let ((rest-of-result
                       ;; 2.91
                       (div-terms (sub-terms L1
                                             (mul-terms (list
                                                         (make-term new-o
                                                                    new-c))
                                                        L2))
                                  L2)))
                  (cons (adjoin-term (make-term new-o
                                                new-c)
                                     (car rest-of-result))
                        (cdr rest-of-result))))))))

  ;; 2.91
  (define (sub-terms l1 l2) ; Hacky way.
    (term-list (sub-poly (make-poly 'x l1)
                         (make-poly 'x l2))))

  ;; 2.91
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (let ((res (div-terms (term-list p1)
                              (term-list p2))))
          (cons (make-poly (variable p1) (car res)) ; The quotient
                (make-poly (variable p1) (cadr res)))) ; The remainder
        (error "Polys not in same var: 
              ADD-POLY"
               (list p1 p2))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: 
              ADD-POLY"
               (list p1 p2))))
  
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))
  
  (define (neg-poly p)
    (define (iter trms)
      (if (empty-termlist? trms)
          trms
          (let ((t1 (first-term trms)))
            (adjoin-term (make-term (order t1) (negate (coeff t1)))
                         (iter (rest-terms trms))))))
    (make-poly (variable p) (iter (term-list p))))

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
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((res (div-poly p1 p2)))
           (list (tag (car res))
                 (tag (cdr res))))))
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (n)
         (empty-termlist? (term-list n))))
  (put 'negate '(polynomial)
       (lambda (p)
         (tag (neg-poly p))))
       
  (put 'negate '(scheme-number) -)
  "Installed polynomial package")

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

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

;; New
(define (negate x) (apply-generic 'negate x))

;;; Tests

(define p (make-polynomial 'x '((3 4) (1 2) (0 1)))) ; 4x^3 + 2x + 1
(define q (make-polynomial 'x '()))
(define r (make-polynomial 'y '()))
(define s (make-polynomial 'y (list (list 2 p) '(1 3))))

(div p p)
(div q p)
(div (make-polynomial 'x '((5 1) (0 -1)))
     (make-polynomial 'x '((2 1) (0 -1))))