#lang sicp

;;; Deps

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))
;;; 2.34

(define 
  (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;;; 2.35

(define (count-leaves t)
  (accumulate (lambda (_ next) (+ 1 next))
              0
              (enumerate-tree t)))

(count-leaves (list 100 (list 200 (list 300 400)) 500))

;; Edit: Doable without enumerate-tree as:
(define (count-leaves-recursive t) 
  (accumulate + 0 
              (map 
               (lambda (t) 
                 (cond ((null? t) 0) 
                       ((pair? t) (count-leaves-recursive t)) 
                       (else 1))) 
               t)))

;;; 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(accumulate-n + 0 s)

;;; 2.37

;; Dep
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (v-in-m) (dot-product v v-in-m))
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v-in-m)
           (matrix-*-vector cols v-in-m))
         m)))

(define v '(2 3 4 5))
(define m '((1 2 3 4)
            (4 5 6 6)
            (6 7 8 9)))
(define n '((1 2 3)
            (2 3 4)
            (3 4 5)
            (4 5 6)))

(matrix-*-vector m v)
(transpose m)
(matrix-*-matrix m n)