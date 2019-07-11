;; Initially I wanted to do operator-last syntax, but that necessitated
;; one icky change: since a procedure's body is no longer the tail of the
;; list, using cdr wouldn't do. We'd need something like cddr instead.

;; I'll just be less ambitious and change lambda expressions to something
;; less verbose.

;; (((x y) => (+ x y)) 1 2)

(define (lambda? exp)
  ((eq? (cadr exp) '=>))
(define (lambda-parameters exp) (caar exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons parameters (cons '=> body)))
