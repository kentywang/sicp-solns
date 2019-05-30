#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;;; Deps

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))

;;; 2.46
(define make-vect cons)	
(define xcor-vect car)	
(define ycor-vect cdr)	
(define (add-vect v1 v2)	
  (make-vect (+ (xcor-vect v1)	
                (xcor-vect v2))	
             (+ (ycor-vect v1)	
                (ycor-vect v2))))	
(define (sub-vect v1 v2)	
  (make-vect (- (xcor-vect v1)	
                (xcor-vect v2))	
             (- (ycor-vect v1)	
                (ycor-vect v2))))	
(define (scale-vect s v)	
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

;;; 2.47

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

;(define origin-frame car)
;(define edge1-frame cadr)
;(define edge2-frame cddr)

;;; 2.48

(define make-segment cons)
(define start-segment car)	
(define end-segment cdr)

;;; 2.49

;; 1.
(define up (make-segment (make-vect 0 0) (make-vect 0 1)))
(define right (make-segment (make-vect 0 1) (make-vect 1 1)))
(define down (make-segment (make-vect 1 1) (make-vect 1 0)))
(define left (make-segment (make-vect 1 0) (make-vect 0 0)))
(segment-painter (list up right down left))

;; 2.
(define stroke1 (make-segment (make-vect 0 0) (make-vect 1 1)))
(define stroke2 (make-segment (make-vect 0 1) (make-vect 0 1)))
(segment-painter (list stroke1 stroke2))

;; 3.
(define north (make-segment (make-vect 0 0.5) (make-vect 0.5 1)))
(define east (make-segment (make-vect 0.5 1) (make-vect 1 0.5)))
(define south (make-segment (make-vect 1 0.5) (make-vect 0.5 0)))
(define west (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))
(segment-painter (list up right down left))

;; 4. Doesn't seem possible to make curved lines with our implementation of segments.
