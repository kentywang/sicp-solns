;; 2.71

;; For n = 5, let
;; a: 1, b: 2, c: 4, d: 8, e: 16

;; Tree from http://community.schemewiki.org/?sicp-ex-2.71:

;;                     {a b c d e} 31
;;                     /           \
;;                {a b c d} 15      e 16
;;                 /     \
;;           {a b c} 7    d 8
;;             /    \
;;        {a b} 3    c 4
;;         /   \
;;      a 1    b 2

;; Most frequent symbol needs 1 bit, least: 4 bits (not 5)
;; For n = 10, least: 9 bits.

;; 2.72
;; Special case (the exponential frequency distribution in 2.71):

;; For the least frequent symbol, there's n - 1 steps down the tree with
;; 1 check step at each node. This looks like O(n) time.

;; More than half the time, though the search will be for the most frequent symbol,
;; which takes 1 step down the tree with n steps (searching left-branch first)
;; or 1 step (searching right-branch first), since intra-tree set sorting leaves the
;; higher-weighted side on the far end of the list. So O(1).

;; This is a unique scenario though, because the 2nd most frequent takes O(n) time,
;; since it has to iterate to end of list to find it.

;; For the mth frequent symbol, there's m steps down, with n - m - 1 steps for checking.
;; So O(mn - m^2) time.

;; So maybe we can simplify and say that it takes O(n^2) for half the time, and O(1)
;; for the other.

;; General case depends on frequency distribution. 
;; With even distribution, we have consistently log(n) tree steps,
;; with each step taking n/2, n/4, etc., so O(log(log n))?