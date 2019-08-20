(f 'x 'y)

Don't need to save/restore envs around any of the operator/operands, since
f is a variable and thus just does a lookup (so no env is changed) and since
x and y are symbols and thus just have their quoted text extracted (so no env
is used).

Similarly, argl doesn't need to be saved/restored for each operand since none
are procedure applications that make use of the register.

Likewise for saving/restoring proc around the operand sequence.

In conclusion, there is nothing that needs preservation for this expression.

((f) 'x 'y)

Since (f) is an application, we'll need to save/restore the original env
before f is applied, as it'll be overwritten by the procedure's env in process
of its application

Everything else is the same as with the first.

(f (g 'x) y)

(g 'x) is an application, so we need to save/restore the env around it (because
the next operand needs the env to do a lookup). We'll also need to preserve the
argl around (g 'x), since it'll be overwritten. For the same reasons, we'll need
to save the proc around the entire operand sequence.

(f (g 'x) 'y)

This is similar to the one above, but we don't need to preserve the env around
(g 'x), since the last operand doesn't use it.