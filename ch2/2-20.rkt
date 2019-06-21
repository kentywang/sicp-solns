#lang sicp

;; Iterative process
(define (same-parity first . rest)
  (define (iter items acc even-first?)
    (if (null? items)
        acc
        (let ((value (car items)))
          (iter (cdr items)
                (if (eq? even-first? ; XNOR
                         (even? value))
                    (append acc (list value))
                    acc)
                get-even?))))
  (iter rest
        (list first)
        (even? first)))

;; time = O(1 + 2 + ... + n) = O(n * (n + 1) / 2) = O(n^2), since each
;; iteration could require the creation of a temporary current list, and there
;; are n iterations.

;; space = O(n), from the new list created.

;; Recursive process
(define (same-parity2 first . rest)
  (define (iter items even-first?)
    (if (null? items)
        nil
        (let ((value (car items)))
          (if (eq? even-first? 
                   (even? value))
              (cons value
                    (iter (cdr items) get-even?))
              (iter (cdr items) get-even?)))))
  (cons first
        (iter rest (even? first))))

;; time = O(n), since the list is created only once at the end.
;; space = O(n), from buildup of cons chain that eventually becomes new list.

;;; Tests

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

(same-parity2 1 2 3 4 5 6 7)
(same-parity2 2 3 4 5 6 7)

;; Edit: Could also keep it iterative and optimal by build list backwards and
;; then reversing it at the end.