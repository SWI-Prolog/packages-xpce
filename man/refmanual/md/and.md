# class and {#class-and}

Class and defines a logical `and` for its associated statements.  An
and object is not just binary but can have any number of arguments,
so one may state

	and(Statement1, Statement2, Statement3)

rather then

	and(Statement1, and(Statement2, Statement3))

When executed,  the and will execute its argument code objects starting
from the first.  It stops with failure as soon as one of the arguments
fails or returns success if all arguments succeed.   And objects are
both used as a logical connective for conditions (see class if) and
as a sequence of statements.

An and object without arguments simply succeeds and is the fastest
succeeding code object.

See also class block, class or and class not.  Class progn defines a
function equivalent of an and.

@see class progn
@see topic Conditions


## Instance variables {#class-and-instvars}

- and<-members: chain
    Code objects that are executed in the order they appear in this
    chain  object.  All members must be executed successfully.  The
    content of the chain may be modified using the chain-methods,
    but the user is responsible that all members of the chain are
    instances of class code.


## Send methods {#class-and-send}

- and->_execute
    Execute the statements of <-members until one fails or the end of the
    chain is reached.  Only in the latter case the and executes
    successfully.

