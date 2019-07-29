4.64

Outranked-by in the body is queried with ?boss bound to another variable, ?who.
This is a rule. The ?middle-manager hasn't been bound in this frame, so the new
unified frame has ?staff-person bound to ?middle-manager, so we are left with
one more variable than when we started in evaluating the inner query.

4.65

The first supervisor clause produces 3 frames binding ?middle-manager to Ben,
Scrooge, and Aull. The second clause filters out Aull since she manages no one,
extends Scrooge's frame with 1 frame since he is manages Robert, and produces 3
extension frames for Ben for each of the people he supervises. 3 + 1 = 4

4.66

Ben realizes that any query that generates duplicate results (such as the wheel
query) will return erroneous accumulations. He could add an intermediate step
after retrieving the stream of frames from qeval to eliminate duplicate frames
(i.e. frames with the exact same bindings).

4.67

Have a globally accessible stack represent the history, where currently
processing queries are on the stack. Each item of the stack is a data structure
storing both the (references to the) pattern being matched and the frame. Before
evaluating each query, we check the items in the stack to see if we have a
duplicate query being run (having the same pattern and frame bindings). This
should catch all infinite loops since they repeat eventually.