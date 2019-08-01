4.71

For disjoin, if the second disjunct is a rule that has as its body a disjoin
with a disjunct that is the rule itself, this could cause an infinite loop.

Similarly for the simple-query, any self-referential rule may cause an infinite
loop.

EDIT: We can get stuck at the time of call if not delayed, but if delayed, we
can get stuck at time of enumerating the stream, which means we should at least
be stuck printing things over and over instead of waiting at an empty terminal.

https://eli.thegreenplace.net/2008/02/09/sicp-sections-442-444
https://wizardbook.wordpress.com/2016/05/15/exercise-4-71/
http://community.schemewiki.org/?sicp-ex-4.71

(assert! (married Minnie Mickey)) 
(assert! (rule (married ?x ?y) 
               (married ?y ?x))) 
(married Mickey ?who) 

(For some reason my evaluator doesn't print anything, delay or not...)

4.72

Interleaves allows the merged streams to be meaningful if the first stream is
infinitely long such that simply appending the first stream to another would
produce nothing more than the first stream again.

Disjoin and stream-flatmap use interleaving as a safeguard so that any
enumerated stream goes through more than just the first stream. This is really
only important for printing, similary to 4.71, since if a stream is actually
finite, interleaving doesn't actually mean much besides reordering things.

So something like this should print elements from the second disjunct, even
though there is no finite solution for the first:

(or (married Mickey ?who)
    (son Adam ?x))