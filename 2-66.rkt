#lang sicp

;;; Deps

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (key record) (car record))
(define (datum record) (cdr record))
(define (make-record key datum) (cons key datum))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
 
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;;; Main

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((curr (entry set-of-records)))
        (cond ((= given-key (key curr))
               curr)
              ((> given-key (key curr))
               (lookup given-key (right-branch set-of-records)))
              (else
               (lookup given-key (left-branch set-of-records)))))))

;;; Tests (from http://wiki.drewhess.com/wiki/SICP_exercise_2.66)
 
(define database
  (list->tree (list (make-record 1 'a)
                    (make-record 5 'e)
                    (make-record 10 'j)
                    (make-record 26 'z))))

(lookup 3 database)
(lookup 10 database)