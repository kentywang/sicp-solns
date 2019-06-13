#lang racket

;;; Deps

(define *the-table* (make-hash));make THE table 
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

(define square (lambda (x) (* x x)))

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

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

;;; Main

;; Strategy: keep raise and chain in global table, use like an generic
;; operator. Chain returns list of types, so an operation using chain could
;; potentially be O(n^2), but it is better than just keeping a number since
;; you can tell if two types are on the same chain this way.

;; Edit: Using raise and chain like generic operators was not the wisest
;; choice. It was a lot of code to make them fit into the apply-generic
;; framework. Next time, I could write a separate apply-generic for them,
;; or try a new approach altogether.

;; 2.83
(define (install-raise-operation)
  (put 'raise '(integer)
       (lambda (n)
         (make-rational n 1)))
  (put 'raise '(rational)
       (lambda (n)
         (cons 'real (/ (numer n) (denom n)))))
  (put 'raise '(real)
       (lambda (n)
         (make-complex-from-real-imag n 0)))
  "Installed raise operation.")

;; 2.84
(define (install-chain-operation)
  (put 'chain '(integer)
       (lambda (n) ; n is the data object. We don't need it.
         (cons 'integer (chain (raise (cons 'integer n))))))
  (put 'chain '(rational)
       (lambda (n)
         (cons 'rational (chain (raise (cons 'rational n))))))
  (put 'chain '(real)
       (lambda (n)
         (cons 'real (chain (raise (cons 'real  n))))))
  (put 'chain '(complex)
       (lambda (n)
         '(complex)))
  "Installed chain operation.")

(define (apply-generic op . args)
  (define (err types)
    (error
     "No method for these types: 
      APPLY-GENERIC"
     (list op types)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1d (length ((get 'chain (list type1)) (contents a1))))
                      (t2d (length ((get 'chain (list type2)) (contents a2)))))
                  (cond ((= t1d t2d) ;; Same depth, but no proc.
                         (err type-tags))
                        ((> t1d t2d) ;; First arg lower.
                         (apply apply-generic
                                (list op ((get 'raise (list type1))
                                          (contents a1)) a2)))
                        ((< t1d t2d) ;; Second arg lower.
                         (apply apply-generic
                                (list op a1 ((get 'raise (list type2))
                                             (contents a2))))))))
              (err type-tags)))))) ;; Not built for more than 2 args.

(define (raise z) 
  (apply-generic 'raise z))
(define (chain z) 
  (apply-generic 'chain z))
                                                                    
;;; More deps

(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
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
  ;; Needed for 2.79
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  'done)

;; Needed for 2.79
(define (numer z)
  ((get 'numer '(rational)) z))
(define (denom z)
  ((get 'denom '(rational)) z))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  ;; From 2.77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (put-coercion source-type target-type proc)
  (put 'coercion (list source-type target-type) proc))

(define (get-coercion source-type target-type)
  (get 'coercion (list source-type target-type)))
  
(define (install-coercion-package) 
  (define (scheme-number->complex n) 
    (make-complex-from-real-imag (contents n) 0)) 
  (define (scheme-number->rational n) 
    (make-rational (contents n) 1)) 
  (put-coercion 'scheme-number 'rational scheme-number->rational) 
  (put-coercion 'scheme-number 'complex scheme-number->complex) 
  "Installed coercion package") 

(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-scheme-number-package)
(install-rational-package)
(install-coercion-package)
(install-raise-operation)
(install-chain-operation)

;;; Tests

(define x '(integer . 2))

(raise x) ; rational
(raise (raise x)) ; real
(raise (raise (raise x))) ; complex

(chain x)
(chain (raise x))
(chain (raise (raise x)))
(chain (raise (raise (raise x))))

(add x (make-complex-from-real-imag 1 1)) ; 3 + i
(add x (make-rational 3 4)) ; 11/4