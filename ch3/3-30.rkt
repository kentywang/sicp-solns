#lang sicp

(define (ripple-carry-adder a b s c)
  (let ((c-in (make-wire))
    (define (iter x y in-c sum)
      (if (null? (cdr x))
          (full-adder (car x) (car y) in-c (car sum) c)
      (let ((c-out (make-wire)))
        (full-adder (car x) (car y) in-c (car sum) c-out)
        (iter (cdr x) (cdr y) c-out (cdr sum)))))
    (iter a b s c-in)))

;; Could map using Fuller adder op and lists:
;; http://community.schemewiki.org/?sicp-ex-3.30

;; Full: 2 half + or
;; Half: and + max(or, and+inv)
;; Ripple: n * (2 and + 2 max (or, and+inv) + or)