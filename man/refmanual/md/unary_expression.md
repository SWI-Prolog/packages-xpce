# class unary_expression {#class-unary_expression}

Arithmetic function of one <-argument.  Its subclasses make an integer
from an expression:

	| round | Round to the nearest integer, halves away from zero |
	| floor | Round down                                          |

xpce numbers need not be integers.  For example, a length in
millimeters is generally not a whole number of pixels, and _/_ does not
truncate.  Where a whole number is needed, wrap the expression:

	send(Window, width, round(W - SBW))

@see class binary_expression


## Instance variables {#class-unary_expression-instvars}

- unary_expression<->argument: expression
    The expression evaluated.


## Send methods {#class-unary_expression-send}

- unary_expression->initialise: argument=expression
    Create from the expression to evaluate.
