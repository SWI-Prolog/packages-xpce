# class progn {#class-progn}

A progn object describes a sequence of statements (code objects) from
which the last must be a function object.  The return value of the progn
(as a function) is the return value of the last function object.

The execution of the progn fails immediately when one of the statements
or the terminating function fails.

Progn (including its name) is inspired by LISP.  See also class and,
class block and class when.

@see class and


## Instance variables {#class-progn-instvars}

- progn<-members: chain
    Chain of statements.  Class progn delegates to this chain object, making
    the chain methods available to manipulate the contents of the progn.
    The user must ensure the types of the chain elements is consistent.


## Send methods {#class-progn-send}

- progn->initialise: statement=code|any ...
    Create a progn object from its statements.  All but the last statement
    should be code objects.  The last may be a function object as well as
    a value.  Class progn is used most commonly to define the implementation
    of get_method objects.


## Get methods {#class-progn-get}

- progn<-_execute: -> unchecked
    If <-members is empty, fail with the error last_is_no_function.
    Otherwise push a variable frame to limit the scope of a local
    `var ->assign`, and execute the statements.  Stop with failure if a
    statement fails.  Finally evaluate the last element if it is a function
    and return either the last element or the result of the evaluation.

