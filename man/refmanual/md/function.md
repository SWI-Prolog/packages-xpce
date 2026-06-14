# class function {#class-function}

A function object is a code object that, when <-execute'd, returns a PCE
data object as the virtual machine instruction get().  Functions are
evaluated iff:

- They appear as part of a code object that is executed

- They are passed to the type-checker (see `type <-check`)
	and the type does not allow for functions to be passed.

Explicit evaluation is achieved using <-execute and <-forward.  Quoting
(e.i. delaying evaluation) is achieved using a quote_function object.

PCE currently provides the following functions:

- Class ? (obtainer)
	An obtainer represents a get() operation.  It evaluates to the
	return value of the get operation represented.

- Class binary_expression
	Super-class for the common arithmetic functions: +, -, *, /

- Class var
	A var represents a PCE variable.  Among these are the predefined
	variables @arg1 ..., @receiver, @event, etc.

- Class when
	A when is a conditional function.  It may be compared to Lisp's
	`if` or C's _Cond ? Expr1 : Expr2_.

- Class progn
	Sequence of statements closed with a function.

NOTE:	Many methods defined on class object reappear on this class.
		The message passing system will, when the receiver is a
		subclass of class function, *not* evaluate the function
		if the method is defined at the level of class function
		or lower in the class hierarchy.

@see type<-check
@see class code


## Get methods {#class-function-get}

- function<-_execute: -> unchecked
    Objects of class function itself cannot be executed.  Yields an error
    message and fails.   Redefined by all subclasses of class function.

- function<-forward: any ... -> unchecked

- function<-_forward: any ... -> unchecked
    Bind @arg1, ...  from the arguments and <-execute the function.  Note
    that @arg1 is a function itself, thus

    	?- get(@arg1, forward, 4, X).		==> X = 4

- function<-convert: quote=quote_function -> function
    Converts an instance of class quote_function into the function
    quoted.  See class quote_function for details.

- function<-execute: -> unchecked
    Explicitly evaluate the function.  See class function itself for the
    default evaluation rules.  Class quote_function avoids evaluation
    of function objects.

    Calls <-_execute to perform the real job.
