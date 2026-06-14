# class binary_expression {#class-binary_expression}

Binary arithmetic expression with <-left and <-right operants.  Class
binary_expression has the subclasses

	| + | Addition                 |
	| - | Subtraction and negation |
- Multiplication
	/		Division

PCE expression objects are first of all designed for supporting
graphics calculations.  The intermediate results are calculated
in fixed-point arithmetic (1/1024 resolution).  The result is
rounded to the nearest integer value.

Binary expressions are used by class = for the definition of equations
used by class spatial.  The are also used by class handle and class
region to express locations in terms of the dimensions of a related
graphical object.  Finally, they may be used as function objects in
methods.

Note that type conversion converts function objects (and thus also
binary expressions) into the value they evaluate too.  This implies that

	send(Box, size, size(W+2, H+2))

Is a comfortable way to do (graphics) calculations in PCE/Prolog.  It
must be noted that

	W1 is W + 2,
	H1 is H + 2,
	send(Box. size, size(W1, H1))

is, in generally faster as the first example creates two PCE expressions
evaluates then and then destroys them again.

**Bugs**:

Evaluation of expressions is written in C, bypassing the message passing
of PCE.  This implies the user cannot define new expression classes.

@see class real
@see class number


## Instance variables {#class-binary_expression-instvars}

- binary_expression<->left: expression
    Operants of the expression.  See class binary_condition for a
    description of the type expression.

- binary_expression<->right: expression
    *Inherits description from*: binary_expression-left


## Send methods {#class-binary_expression-send}

- binary_expression->initialise: left=expression, right=expression
    Create the expression from its <-left and <-right operator.  As class
    binary_expression itself does not define evaluation there is little use
    in creating instances of this class.


## Get methods {#class-binary_expression-get}

- binary_expression<-value: binding== ... -> value=int|number|real
    Determine the value of the expression given the values for the var
    objects appearing in it.  Used by class handle and class region.
    Example:

    	?- get(x + 5, value, x=3, X).       ==> X = 8

    Note: in this example we can simply write `x` instead of new(X,
    var(x)) as x is the name of a predefined var object.  See class
    var.

    See also `=<-var`.

- binary_expression<-var_in: variable=var -> number=int
    Count the number of occurrences of the var object in the expression.
    Used by `= <-var`.

