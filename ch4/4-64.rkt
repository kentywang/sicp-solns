#lang racket

Outranked-by in the body is queried with ?boss bound to another variable, ?who.
This is a rule. The ?middle-manager hasn't been bound in this frame, so the new
unified frame has ?staff-person bound to ?middle-manager, so we are left with
one more variable than when we started in evaluating the inner query.