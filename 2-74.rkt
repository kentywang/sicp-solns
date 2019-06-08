#lang racket

;;; 2.74

;;; Deps

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

(define *the-table* (make-hash));make THE table 
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

;;; 1. & 2.

;; Given:
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: 
             APPLY-GENERIC"
           (list op type-tags))))))

;; We can specify:
(define (get-record ee-name division-name) 
  (apply-generic 'get-record
                 (attach-tag division-name ee-name)))

(define (get-salary ee-name division-name) 
  (apply-generic 'get-salary
                 (attach-tag division-name ee-name)))

;; Each division will need to provide an interface like this:
(define (install-southwest-branch-records)
  ;; Set of employees. This division chooses to implement it this way:
  ;; (This is also the internal selector.)
  (define (get-ee name)
    (cond ((eq? name 'bob-smith)
           '((salary 12300) (address "101 Main St")))
          ((eq? name 'jane-doe)
           '((salary 45600) (address "23 High Ave")))
          (else
           false)))
  (define (get-ee-sal name)
    (cadar (get-ee name)))
  ;; Interface
  (put 'get-record '(southwest-branch) get-ee)
  (put 'get-salary '(southwest-branch) get-ee-sal)
  'done)

;; Another fixture for testing
(define (install-central-branch-records)
  (define (get-ee name)
    (cond ((eq? name 'steve-jobs)
           '((salary 1) (address "Infinite Loop")))
          ((eq? name 'jony-ive)
           '((salary 999999) (address "UK")))
          (else
           false)))
  (define (get-ee-sal name)
    (cadar (get-ee name)))
  (put 'get-record '(central-branch) get-ee)
  (put 'get-salary '(central-branch) get-ee-sal)
  'done)

(install-southwest-branch-records)
(install-central-branch-records)

;; So that the admin can just call this:
(get-record 'bob-smith 'southwest-branch)
(get-record 'jane-doe 'southwest-branch)

(get-salary 'jony-ive 'central-branch)

;; 1. Each division is responsible for providing an interface procedure for the
;; retrieve-employee-record operation with the type tag of that division's
;; name. This procedure retrieves the employee data from the division's
;; internal representation of the personnel records.

;; 2. The record can be structured in any way as long as the division can use
;; internal methods of finding it.

;;; 3.

(define (find-employee-record name divisions)
  (if (null? divisions)
      false
      (or (get-record name (car divisions))
          (find-employee-record name (cdr divisions)))))

;; Test
(find-employee-record 'jane-doe '(central-branch southwest-branch))

;; 4. The taken over company just needs to be associated with a new type tag
;; and ordered to provide procedures to retrieve its employees' records and
;; salaries.