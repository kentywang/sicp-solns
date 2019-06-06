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

;;; Main

(define (successive-merge pairs)
  (cond ((null? pairs) '())
        ((= 1 (length pairs)) (car pairs))
        (else
         (successive-merge (adjoin-set (make-code-tree (car pairs)
                                                       (cadr pairs))
                                       (cddr pairs))))))

;;; Tests

(generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))