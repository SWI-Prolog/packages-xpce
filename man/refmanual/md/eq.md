# class eq {#class-eq}

Binary_condition object that succeeds if <-left and <-right evaluate to
the same numeric value.   See class binary_condition for details.

Besides acting as a simple test, it can also solve equations with a
single variable.  See <-var.

@see topic Conditions


## Send methods {#class-eq-send}

- eq->_execute
    Evaluate both expressions to a numeric value and succeed if they
    evaluate to the same value.


## Get methods {#class-eq-get}

- eq<-var
    Regards the = object as an equation to be solved.  It can determine the
    value of a variable occuring only once in the equation if all other
    variables are given.  This facility is used by class spatial to express
    spatial relations between graphical objects.   Below are some examples:

    	?- get(4+x=7, var, x, X)		==> X = 3
    	?- get(5*x=y, var, y, x=3, Y)	==> Y = 15

    Note that `x`, `y`, `w`, `h` and some more are the names of predefined
    var objects and thus `x` is translated to this var object by PCE's
    type-checking system.  The binding of the var objects in an = object are
    not affected by this method.

    See also class var and class spatial.

    @see class spatial

