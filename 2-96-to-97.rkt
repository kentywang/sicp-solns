#lang racket

(define *the-table* (make-hash));make THE table 
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

(define square (lambda (x) (* x x)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

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
  ;; 2.97: 2.
  (define (make-rat n d)
    (let ((rnd (reduce n d)))
      (cons (car rnd) (cadr rnd))))
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

  ;; 2.96: 1.
  (define (pseudoremainder-terms a b)
    (let ((c (coeff (first-term b)))
          (o1 (order (first-term a)))
          (o2 (order (first-term b))))
      (let ((factor (make-term
                     0
                     (expt c (+ 1 (- o1 o2))))))
        (remainder-terms (mul-term-by-all-terms factor a)
                         b))))

  ;; 2.96: 1.
;  (define (gcd-terms a b)
;    (if (empty-termlist? b)
;        a
;        (gcd-terms b (pseudoremainder-terms a b))))

  ;;2.96: 2.
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (simplify-terms (append a b))
        (gcd-terms b (pseudoremainder-terms a b))))

  (define (simplify-terms ts)
    (let ((factor (coeff (accumulate (lambda (a b)
                                       (make-term 0
                                                  (gcd (coeff a)
                                                       (coeff b))))
                                     (first-term ts)
                                     (rest-terms ts)))))
      (map (lambda (t)
             (make-term (order t) (/ (coeff t) factor)))
           ts)))
      
;  (define (gcd-terms a b)
;    (if (empty-termlist? b)
;        a
;        (gcd-terms b (remainder-terms a b))))

  ;; 2.97: 1.
  (define (reduce-terms n d)
    (let ((gcdiv (gcd-terms (term-list n) (term-list d))))
      (let ((fact (make-term 0
                             (expt (coeff (first-term gcdiv))
                                   (+ 1 (- (max (order (first-term n))
                                                (order (first-term d)))
                                           (order (first-term gcdiv)))))))
            (int-gcd (simplify-terms (append n d))))
        (let ((new-n (mul-term-by-all-terms fact n))
              (new-d (mul-term-by-all-terms fact d)))
;          (print (list "fact:" fact))(newline)
;          (print (list "int-gcd" int-gcd))(newline)
;          (print (list "gcdiv" gcdiv))(newline)
;          (print (list "new-n" new-n))(newline)
;          (print (list "new-d" new-d))(newline)
          (list (car (div-terms (car (div-terms new-n gcdiv)) int-gcd))
                (car (div-terms (car (div-terms new-d gcdiv)) int-gcd)))))))

  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (let ((rt (reduce-terms (term-list p1)
                                 (term-list p2))))
          (list (make-poly (variable p1) (car rt))
                (make-poly (variable p2) (cadr rt))))
        (error "Polys not in same var: 
              REDUCE-POLY" p1 p2)))
     
  ;; 2.94
  (define (gcd-poly a b)
    (if (same-variable? (variable a) (variable b))
        (make-poly (variable a)
                   (gcd-terms (term-list a)
                              (term-list b)))
        (error "Polys not in same var: 
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
              DIV-POLY"
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
  ;; 2.97: 2.
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (reduce-poly p1 p2))))
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

;; 2.97: 2.
(define (reduce-integers n d)
  (let ((g (gcd-int n d)))
    (list (/ n g) (/ d g))))
(put 'reduce '(scheme-number scheme-number) reduce-integers)

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

(define (reduce a b)
  (apply-generic 'reduce a b))

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

;;; 2.96

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

;;; 2.97

(define p4
  (make-polynomial 'x '((1 1) (0 1))))
(define p5 
  (make-polynomial 'x '((3 1) (0 -1))))
(define p6 
  (make-polynomial 'x '((1 1))))
(define p7 
  (make-polynomial 'x '((2 1) (0 -1))))
(define rf1 (make-rational p4 p5))
(define rf2 (make-rational p6 p7))
(add rf1 rf2)