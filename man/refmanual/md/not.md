# class not {#class-not}

Class `not` defines negation of conditional statements.  When executed,
it executes its argument and fails if the argument is executed with
success.  See class `code` for more general information on executable
objects.

@see class code
@see class and
@see class or
@see topic Conditions


## Instance variables {#class-not-instvars}

- not<->argument: code
    Conditional code object whose result is to be negated.


## Send methods {#class-not-send}

- not->initialise: test=code
    Create from a single conditional test.

- not->_execute
    Execute the argument and succeed if and only if it fails.
