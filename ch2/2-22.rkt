#lang sicp

;; Since the iterative procedure wraps each previous result with another layer
;; of cons, it is adding new elements to the beginning of the list, even though
;; it traverses through the list in forward order.

;; The rewritten iterative procedure doesn't work. The first cons creates a
;; pair with the first element as the empty list. The next iteration makes
;; this pair the first element of a new pair. Thus, it creates a reverse
;; version of a list (on top of the fact that the items are still in reverse
;; order.