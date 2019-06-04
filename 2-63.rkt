; 1. Both procedures return the same list.
; 2. Copy to tree only conses once per element, so O(n), whereas
; the first procedure must repeatedly append the left subtree of
; every node, resulting in O(n^2) steps in the worst case of only
; left subtrees and no right subtrees.

; Note: second procedure uses an interesting technique I've seen
; before to start at end to list working backwards, I should investigate.

; Edit: Online solutions say the first procedure is O(n log n).