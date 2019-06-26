#lang sicp

;; Delay time is 2 * inverter-delay + and-gate-delay.

(define (or-gate o1 o2 output)
  (let ((a1 (make-wire))
        (a2 (make-wire))
        (i (make-wire)))
    (inverter o1 a1)
    (inverter o2 a2)
    (and-gate a1 a2 i)
    (inverter i output)
  'ok))