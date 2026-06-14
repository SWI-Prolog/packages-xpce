# class or {#class-or}

Class or defines a logical `or` for its associated statements.  An
`or` is not just binary but can have any number of arguments.

When executed, `or` will execute its argument code objects starting
from the first.  It stops with success as soon as one of the arguments
succeeds or returns failure if no argument succeed.  An or
without arguments simply fails.

See also class if, class and and class not.

@see topic Conditions


## Instance variables {#class-or-instvars}

- or<-members: chain
    List of disjunctive statements.  Note that class or delegates to this
    chain, so all methods applicable to chains may be used to manipulate the
    set of statements in the or.


## Send methods {#class-or-send}

- or->_execute
    Execute the code objects from <-members until one succeeds or the end of
    the chain is reached.  Succeeds in the first case and fails in the
    second.

- or->initialise: test=code ...
    Create an or object from a series of statements.  Or objects are almost
    exclusively used to realise disjunction in conditions for if objects or
    while objects.

