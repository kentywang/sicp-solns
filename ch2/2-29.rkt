#lang sicp

;;; Deps

(define (make-mobile2 left right)
  (list left right))

(define (make-branch2 length structure)
  (list length structure))

;;; 1.

(define left-branch car) ;; Or (list-ref 0).
(define right-branch cdr) ;; Or (list-ref 1).

(define branch-length car)
(define branch-structure cdr)

;;; 2.

;; Using a helper inner def
(define (total-weight mobile)
  (define (branch-weight branch)
    (let ((structure (branch-structure branch)))
      (if (pair? structure)
          (+ (branch-weight (left-branch structure))
             (branch-weight (right-branch structure)))
          structure)))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; time = O(n)
;; space = O(n)

;;; 3.

(define (torque branch)
  (let ((structure (branch-structure branch)))
    (* (branch-length branch)
       (if (pair? structure)
           (total-weight structure)
           structure))))

;; Trying without a helper inner def
(define (balanced?2 mobile)
  (if (pair? mobile)
      (let ((l (left-branch mobile))
            (r (right-branch mobile)))
        (and (= (torque l)
                (torque r))
             (and (balanced?2 (branch-structure l))
                  (balanced?2 (branch-structure r)))))
        true)) ;; Weight is always balanced.

;; time = O(n^2), since redundant torque calls.
;; space = O(n)

;; Faster implementation:
(define (balanced? mobile)
  (define (print-torques a b)
    (display "Left torque: ")
    (display a)
    (newline)
    (display "Right torque: ")
    (display b)
    (newline))
  (define (weight-if-balanced m)
    (let ((l (left-branch m))
          (r (right-branch m)))
      (let ((sl (branch-structure l))
            (sr (branch-structure r))
            (ll (branch-length l))
            (lr (branch-length r)))
        (let ((wl (if (pair? sl)
                      (weight-if-balanced sl)
                      sl))
              (wr (if (pair? sr)
                      (weight-if-balanced sr)
                      sr)))
          (let ((tl (* ll wl))
                (tr (* lr wr)))
            (print-torques tl tr)
            (if (and (not (= tl 0))
                     (not (= tr 0))
                     (= tl tr))
                (+ wl wr) ; Return sum of weights, not torque.
                0)))))) ; Only way to notify call stack below that unbalanced?
  (not (= 0 (weight-if-balanced mobile))))

;; time = O(n)
;; space = O(n)

;;; 4. If we changed the constructors to this:

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;; The only thing we'd need to modify with the selectors is changing the cadr
;; to cdr. Everything else stays the same.

;;; Tests
(define x (make-mobile (make-branch 9 3)
                       (make-branch 3 (make-mobile (make-branch 10 3)
                                                   (make-branch 5 6)))))

;; [(2L 3W) (3L [(3L 4W) (5L 6W)])] mobiles are brackets, branches are parens