# class assign {#class-assign}

Class assign defines an assignment statement for a var object.  An
assign is an executable object that, when executed, evaluates its
<-value and assigns its <-var with this value.  See class var for
a discussion on variables and assignments (`var ->assign`).

Class assign is first of all introduced for syntactical cosmetical
reasons.  The following two code objects perform the same function:

	assign(Var, Value).
	message(Var, assign, Value).

An assign object demands its first argument to be an instance of class
var.  This implies that named variables can be referred to much more
elegant.  Executing

	assign(x, 10)

Will assign 10 to the variable named `x`, while the message variant
would be:

	message(?(@variables, member, x), assign, 10)

See also <-convert.

@see class var


## Instance variables {#class-assign-instvars}

- assign<->scope: {local,outer,global}
    Scope of the assignment.  See `var ->assign` for the interpretation of
    the scope names.  The default is `local`.

- assign<->value: any|function
    Value to assign to the variable.  In many cases this will be a function
    object, which is executed by the assign object when it is ->_execute'd.

- assign<->var: var
    Var object to assign.  See also <-value and `var ->assign`.


## Send methods {#class-assign-send}

- assign->_execute
    The execution of an assign object implies

    1. Evaluate <-value if this is a function object.

    2. `var ->assign` the <-var with the result of 1).

- assign->initialise: variable=var, value=any|function, scope=[{local,outer,global}]
    Create a var object from the <-var, <-value and <-scope.

