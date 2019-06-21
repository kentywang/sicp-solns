#lang racket
(require "2-2.rkt")

;; Many ways to input or store the data for a rectangle:
;; - width, height, point (could be starting pt, center, or offset)
;; - 4 points (must be at right angles)
;; - 2 points (won’t know if rectangle should be tilted, can’t be on same axis)
;; - line, width
;; - four lines (must connect, be at right angles)

;; The more information we store for the rectangle, the less reliance on
;; lower-level (points, lines) data from higher-level code.

;; With our knowledge currently, the only compound data object we know of are
;; pairs, but let’s go with the 3-value approach.
;; Downside is that procedures that manipulate rectangles might need to go to
;; lower-level language of points and lines.

(define (make-rectangle width height offset)
  (cons width (cons height offset)))         ; (width, (height, offset))

(define (rect-h r)
  (car (cdr r)))

(define (rect-w r)
  (car r))

;;; Another representation

(define (make-rectangle-2 tl tr bl br)
  (cons (cons tl tr) (cons bl br)))          ; ((tl, tr), (bl, br))

(define (rect-h-2 r)
  ;; Pick the left two points to compare.
  (let ((top (car (car r)))
        (bot (car (cdr r))))
    (- (y-point top)
       (y-point bot))))

(define (rect-w-2 r)
  ;; Pick the top two points to compare.
  (let ((top (car r)))
    (let ((left (car top))
          (right (cdr top)))
      (- (x-point right)
         (x-point left)))))

;;; Higher layer that interfaces with rect

(define (area r)
    (* (rect-h r)
       (rect-w r)))

(define (perimeter r)
  (+ (* (rect-h r)
        2)
     (* (rect-w r)
        2)))

;;; Same procedures, but now for the second representation

(define (area-2 r)
    (* (rect-h-2 r)
       (rect-w-2 r)))

(define (perimeter-2 r)
  (+ (* (rect-h-2 r)
        2)
     (* (rect-w-2 r)
        2)))

;;; For testing

(let ((r
       (make-rectangle 20 10 0)))
  (display (area r))
  (newline)
  (display (perimeter r))
  (newline))

(let ((r
       (make-rectangle-2
         (make-point 0 10)
         (make-point 20 10)
         (make-point 0 0)
         (make-point 20 0))))
  (display (area-2 r))
  (newline)
  (display (perimeter-2 r))
  (newline))