(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; would become:

(let ((y '*unassigned*)
      (dy '*unassigned*))
  (let ((a (integral (delay dy) y0 dt))
        (b (stream-map f y)))
    (set! y a)
    (set! dy b))
  y)

;; The steps with normal sequential evaluation is:
;; 1. y: (integral (delay dy) y0 dt) - okay since delay is there.
;; 2. dy: (stream-map f y) - okay since y is now defined.

;; With the original pseudo-simultaneous approach:
;; 1. y: *unassigned*
;; 2. dy: *unassigned*
;; 3. y: (integral (delay dy) y0 dt) - okay since delay is there.
;; 4. dy: (stream-map f y) - okay since y is now properly assigned.

;; This new approach:
;; 1. y: *unassigned*
;; 2. dy: *unassigned*
;; 3. a: (integral (delay dy) y0 dt) - okay since delay is there.
;; 4. b: (stream-map f y) - error since y is not stream.
;; 5. y: a - assuming error handled, okay.
;; 6. dy: b - assuming error handled, okay.

;; So the problem is that we can't stream-map over *unassigned*. With the
;; original pseudo-simultaneous approach, it should work though.