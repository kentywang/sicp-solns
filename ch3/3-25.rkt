#lang sicp

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (assoc key records)
      ;; (display records)
      (cond ((null? records) false)
            ;; Since we call assoc now on "records" that may actually be
            ;; just a value, we need to make sure we stop if it's an atom.
            ((not (pair? records)) false)
            ((eq? key (caar records))
             (car records))
            (else (assoc key (cdr records)))))

    (define (lookup keys r)
      (if (null? keys)
          (cdr r)
          (let ((subr
                 (assoc (car keys) (cdr r))))
            (if subr
                (lookup (cdr keys) subr)
                false))))

    (define (insert! keys value r)
      ;; (display "Insert: keys: ")
      ;; (display keys)
      ;; (display ", val: ")
      ;; (display value)
      ;; (display ", lvl: ")
      ;; (display (car r))
      ;; (newline)
      (let ((subr
             (assoc (car keys) (cdr r))))
        (if subr
            (if (null? (cdr keys))    ; Is this the last key?
                (set-cdr! subr value) ; If so, set the value!
                (insert! (cdr keys) value subr))
            (begin (set-cdr!
                    r
                    (cons (list (car keys))
                          (cdr r)))
                   (insert! keys value r))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc)
             (lambda (keys)
               (if (not (null? keys))
                   (lookup keys local-table))))
            ((eq? m 'insert-proc!)
             (lambda (keys value)
               (if (not (null? keys))
                   (insert! keys value local-table))))
            (else (error "Unknown operation:
                          TABLE" m))))
    dispatch))

;;; Tests

(define test-tbl (make-table))
(define get (test-tbl 'lookup-proc))
(define put (test-tbl 'insert-proc!))

(put '(a) "Hello")
(put '(a 1) "World!")
(put '(a 1 x) "Bye")
(put '(a 1 y) "Hola")
(put '(b 2 z) "Ciao")
(get '(a))
(get '(a 1))
(get '(a 1 x))
(get '(a 1 y))
(get '(b 2 z))
