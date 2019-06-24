#lang sicp

(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records))
             (car records))
            (else (assoc key (cdr records)))))

    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr!
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr!
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation:
                          TABLE" m))))
    dispatch))

;;; Tests

(define test-tbl (make-table equal?))
(define get (test-tbl 'lookup-proc))
(define put (test-tbl 'insert-proc!))

(put 'a 1 "Hello")
(put 'a 9 "World!")
(put 'b 5 "Bye")
(put '(c d) 3 "Ruh-roh")
(get 'a 1)
(get 'a 9)
(get 'b 5)
(get 'b 1)
(get 'c 2)
(get '(c d) 3)
