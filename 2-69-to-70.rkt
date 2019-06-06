#lang racket
(require "2-67-to-68.rkt")

;;; Deps

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

;;; 2.68

(define (successive-merge pairs)
  (cond ((null? pairs) '())
        ((= 1 (length pairs)) (car pairs))
        (else
         (successive-merge (adjoin-set (make-code-tree (car pairs)
                                                       (cadr pairs))
                                       (cddr pairs))))))

;;; Tests

(generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

;;; 2.69

(define t
  (generate-huffman-tree
   '((A 2)
     (NA 16)
     (BOOM 1)
     (SHA 3)
     (GET 2)
     (YIP 9)
     (JOB 2)
     (WAH 1))))

(define m
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP
    YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

(length (encode m t))

;; 1. 84 symbols.
;; 2. 8 symbols, so log2(8) = 3 bits. The message would be 3 * 36 = 108 bits long.