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

4.73

We should note that we're dealing with interleaving a stream of streams with
itself rather than interleaving two separate streams.

The issue with trying to use a normal interleaving procedure in flattening a
stream of streams is that we will pass the evaluation of the flattening
procedure on the rest of the stream to the interleaving procedure as its
second argument. This means the shape of the process is to immediately expand
to a nesting of interleave calls instead of having one interleave call work
through two streams at a time. Also, since we must unfurl all the nested
flatten calls out to interleaves, we must process the last elements first,
meaning if the stream is infinite, we won't ever return (and also defeats
the purpose of streams).

By delaying the second argument to interleave, we can have a controlled
processing of interleaving so that we can process a possibly-infinite stream
to be flattened in a more front-to-back sequence. This will thus allow
interleave applications to be "interleaved" with flatten applications, rather
than flattening all first before interleaving. (Will there still be deeply
nested interleave calls though?)