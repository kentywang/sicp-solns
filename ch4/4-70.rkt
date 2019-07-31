#lang racket

;;; 4.70

;; (set! THE-ASSERTIONS (cons-stream assertion THE-ASSERTIONS) would return
;; a stream of just that one assertion repeated infinitely, because the
;; evaluation of THE-ASSERTIONS is delayed to when it is accessed, and at that
;; time its binding is not to the original stream of assertions anymore, but to
;; (cons-stream assertion THE-ASSERTIONS). A let allows us to specify exactly
;; that we want to cons the new assertion to the stream of the old assertions.