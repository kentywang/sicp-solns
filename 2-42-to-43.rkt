#lang sicp

;;; Deps

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;; Modified to be zero-indexed.
(define (queens board-size)
  (define (queen-cols k)
    (if (< k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  0 
                  board-size)))
          (queen-cols (- k 1))))))
  (accumulate (lambda (_ next) ; Counting number of solutions
                (inc next))
              0
              (queen-cols board-size)))

;; time = O(n^7)?
;; adjoin called n^2 times, & each adjoin needs at most 2n additional calls.
;; flatmap uses append, so another n^2.
;; Then, for each result, running safe costs n^2.

;; space = O(n^n), at least, since we could possibly have n^n results to hold.

;;; 2.42 Main

(define (timer f)
  (lambda (x)
    (let ((start (runtime)))
      (let ((output (f x)))
        (display "Runtime: ")
        (display (/ (- (runtime) start)
                    100000.))
        (newline)
        output))))

(define empty-board
  (list (list "." "." "." "." "." "." "." ".")
        (list "." "." "." "." "." "." "." ".")
        (list "." "." "." "." "." "." "." ".")
        (list "." "." "." "." "." "." "." ".")
        (list "." "." "." "." "." "." "." ".")
        (list "." "." "." "." "." "." "." ".")
        (list "." "." "." "." "." "." "." ".")
        (list "." "." "." "." "." "." "." ".")))

(define (print board)
  (cond ((null? board)
         (newline))
        (else
         (display (car board))
         (newline)
         (print (cdr board)))))

;; Like list-ref, but returns pair with element, not just element itself.
(define (iter-rows times curr)
      (if (= times 0)
          curr
          (iter-rows (dec times) (cdr curr))))
        
;((. . . . . Q . .)
; (. . Q . . . . .)
; (Q . . . . . . .)
; (. . . . . . Q .)
; (. . . . Q . . .)
; (. . . . . . . Q)
; (. Q . . . . . .)
; (. . . Q . . . .))

(define (adjoin-position row col rest-of-queens)
  (define (iter-row times qs) ; Find row to add new queen.
    (if (= times 0)
        (cons (iter-col col (car qs)) ; Return new row that will be made,
              (cdr qs))               ; with the rest of the rows as the cdr.
        (cons (car qs) ; Build up rows above it, since board is immutable.
              (iter-row (dec times) (cdr qs)))))
  (define (iter-col times row)
    (if (= times 0)
        (cons "Q"
              (cdr row))
        (cons (car row)
              (iter-col (dec times) (cdr row)))))
  (iter-row row rest-of-queens))

;;; Testing adjoin-position

(define test-board
  (adjoin-position 7 4 (adjoin-position 4 7 (adjoin-position 3 0 empty-board))))
(print test-board)

(define (safe? col qs)
  
  (define (find-new-queen row)
    (if (or (= row 7) ; Reached last row, so queen must be here.
            (eq? (list-ref (list-ref qs row) col) ; Check qs[col][row] == "Q".
               "Q"))
        row
        (find-new-queen (inc row))))
  
  (define (empty-top-diag? row col qs) ; Start at the toprightmost.
    (define (iter-diag col-idx curr)
      (cond ((= col-idx col) ; Don't need to check both col and row.
             true)
            ((eq? (list-ref (car curr) col-idx) ; Check col index of current row.
                  "Q")
             false)
            (else
             (iter-diag (inc col-idx) (cdr curr))))) ; Iter to next row.
    (let ((diff (- row col)))
      (if (> diff 0)
          (iter-diag 0 (iter-rows diff qs)) ; row > col
          (iter-diag (- diff) qs)))) ; col > row
                  
  (define (empty-bot-diag? row col qs)
    (define (iter-diag col-idx curr)
      (cond ((or (null? curr)
                 (< col-idx 0))
             true)
            ((eq? (list-ref (car curr) col-idx)
                  "Q")
             false)
            (else
             (iter-diag (dec col-idx) (cdr curr))))) ; Next row, earlier col idx.
    (iter-diag (- col 1) (iter-rows (+ row 1) qs)))
  
  (define (empty-row? row col qs)
    (define (iter times curr)
      (cond ((= times 0)
             true)
            ((eq? (car curr) "Q")
             false)
            (else
             (iter (dec times) (cdr curr)))))
    (iter col (list-ref qs row)))
  
  (let ((row (find-new-queen 0)))
    (and (empty-row? row col qs)
         (empty-top-diag? row col qs)
         (empty-bot-diag? row col qs))))

;; Test
((timer queens) 7)

;; Edit: Could implement board as a sequence of numbers instead of a sequence
;; of sequences.
;; http://wiki.drewhess.com/wiki/SICP_exercise_2.42

;; 2.43
;; queens-col gets needlessly called multiple times, duplicating work that was
;; already solved. queens-col gets called n times the necessary amount, each
;; which calls itself n times the necessary amount, so resulting in n^n
;; increased runtime. O(T^T)

;; Edit: O(T^7)?
;; http://wiki.drewhess.com/wiki/SICP_exercise_2.43