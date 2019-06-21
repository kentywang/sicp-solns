#lang racket

;; Generic operations with explicit dispatch: needs to modify each existing
;; procedure to add a new type. A new operation needs to handle dispatches on
;; existing types too.

;; Data-directed style: needs the set of type-specific operations in the table
;; for a new type. For a new operation, it needs a set of new operations that
;; work on every type. No modifications on previously written procedures are
;; needed.

;; Message-passing style: A new type needs to handle dispatches on existing
;; operations. A new operation requires modifying the representation for each
;; data object to add a new case.

;; For a system where new types are added but no new operations are, message-
;; passing or data-directed are the preferred styles.

;; For a system where new operations must often be added, generic operations or
;; data-directed work well.