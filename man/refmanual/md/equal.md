# class equal {#class-equal}

Test for equality.  Succeeds if both <-left and <-right are equal
(compared by reference).   The following example returns the
first object called gnu in Chain:

	get(Chain, find, @arg1?name == gnu, Gnu)

See also class \== and `object ->equal`.

@see object->equal
@see class \==
@see topic Conditions
@see object->is_on
@see object->is_off
@see object->has_value


## Instance variables {#class-equal-instvars}

- equal-left
    Operant.  If it is a function object, it will be evaluated by
    ->_execute.

- equal-right
    *Inherits description from*: ==-left


## Send methods {#class-equal-send}

- equal->_execute
    If either or both operant is a function, evaluate it.  Next succeed if
    the references of both operants is the same.

- equal->initialise
    Create from <-left and <-right operant.  Either or both may be a
    function, which is evaluated by ->_execute.

