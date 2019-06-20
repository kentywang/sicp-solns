#lang racket

(define *the-table* (make-hash));make THE table 
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

(define square (lambda (x) (* x x)))

;;; Main

;; No modifications.
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number) ; New case for numbers
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number) ; New case for numbers
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum) ; New case for numbers
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: 
              CONTENTS" datum))))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  'done)

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)



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

  ;; 2.94
  (define (remainder-terms x y)
    (cadr (div-terms x y)))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))

  ;; 2.94
  (define (gcd-poly a b)
    (if (same-variable? (variable a) (variable b))
        (make-poly (variable a)
                   (gcd-terms (term-list a)
                              (term-list b)))
        (error "Polynomials not of same primary variable:
              GCD-POLY" a b)))

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
  (put 'gcd '(polynomial polynomial)
       (lambda (a b)
         (tag (gcd-poly a b))))
  (put 'negate '(scheme-number) -)
  "Installed polynomial package")

(define (install-zero-package)
  ;; (zero? x) is like (= x 0)
  (put '=zero? '(scheme-number) zero?)
  (put '=zero? '(rational) (lambda (x) (zero? (numer x))))
  (put '=zero? '(complex) (lambda (x) (zero? (magnitude x))))
  "Installed zero package.")

;; 2.94
(define (gcd-int a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(put 'gcd '(scheme-number scheme-number) gcd-int)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (numer z)
  ((get 'numer '(rational)) z))
(define (denom z)
  ((get 'denom '(rational)) z))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (negate x) (apply-generic 'negate x))
(define (=zero? y) (apply-generic '=zero? y))

(define (greatest-common-divisor a b)
  (apply-generic 'gcd a b))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: 
             APPLY-GENERIC"
           (list op type-tags))))))

(install-scheme-number-package)
(install-rational-package)
(install-polynomial-package)
(install-zero-package)

;;; Tests

(define p1 
  (make-polynomial 
   'x '((2 1) (1 -2) (0 1))))

(define p2 
  (make-polynomial 
   'x '((2 11) (0 7))))

(define p3
  (make-polynomial 
   'x '((1 13) (0 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)