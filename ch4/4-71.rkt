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