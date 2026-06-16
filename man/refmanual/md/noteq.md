# class noteq {#class-noteq}

Test non equivalence of objects.  X \== Y is equivalent to not(X == Y).
See also class ==, class not and `object ->equal`.

@see object->equal
@see topic Conditions
@see class ==
@see object->has_value


## Instance variables {#class-noteq-instvars}

- noteq-left
    Operant.  If it is a function object it will be evaluated bu ->_execute.

- noteq-right
    *Inherits description from*: \==-left


## Send methods {#class-noteq-send}

- noteq->_execute
    If either or both operant is a function, evaluate it.  Next succeed if
    the references of both operants are not the same.

- noteq->initialise
    Create from <-left and <-right operant.  Either or both may be a
    function, which is evaluated by ->_execute.

