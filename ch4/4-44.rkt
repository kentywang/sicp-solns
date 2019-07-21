#lang sicp

;;; Deps

(define (require p)
  (if (not p) (amb)))

(define (safe? curr rows)
  (define len (length rows))
  (define (recur index rs)
    (if (null? rs) ; Terminate with true if no rows to check.
        true
        (and (not (= curr (car rs))) ; Can't share same row.
             (not (= (abs (- curr (car rs))) ; Can't be on same diag.
                     (- len index)))
             (recur (+ 1 index) (cdr rs)))))
  (recur 0 rows))
             

;;; Main

(define (queens)
  (let ((a (amb 1 2 3 4 5 6 7 8))
        (b (amb 1 2 3 4 5 6 7 8)))
    (require (safe? b (list a)))
    (let ((c (amb 1 2 3 4 5 6 7 8)))
      (require (safe? c (list a b)))
      (let ((d (amb 1 2 3 4 5 6 7 8)))
        (require (safe? d (list a b c)))
        (let ((e (amb 1 2 3 4 5 6 7 8)))
          (require (safe? e (list a b c d)))
          (let ((f (amb 1 2 3 4 5 6 7 8)))
            (require (safe? f (list a b c d e)))
            (let ((g (amb 1 2 3 4 5 6 7 8)))
              (require (safe? g (list a b c d e f)))
              (let ((h (amb 1 2 3 4 5 6 7 8)))
                (require (safe? h (list a b c d e f g)))
                (list a b c d e f g h)))))))))