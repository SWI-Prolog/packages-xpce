# class number {#class-number}

A number object represents an integer just like the primitive type
`int`.  Numbers are more costly, but allow for some operations such as
comparison or addition.

Instances of class number are commonly used to implement counters:

	count_instances(Device, Class, Instances) :-
		new(I, number(0)),
		send(Device, for_all, @default,
			 if(message(@arg1, instance_of, Class),
			    message(I, plus, 1))),
		get(I, value, Instances),
		send(I, done).

See also class real.

@see class binary_expression
@see pce<-max_integer


## Instance variables {#class-number-instvars}

- number-value: alien:long
    Represented value.  This is an `int` and thus limited to the range

    	`@pce <-min_integer` .. `@pce <-min_integer`


## Send methods {#class-number-send}

- number->minus: int|number
    Subtract argument from value.

- number->plus: int|number
    Add argument to value.

- number->times: int|number
    Multiply value by argument.

- number->divide: int|number
    Basic arithmetic operations.  Note that arithmetic may also be
    performed by the function classes /, +, - and *.   Numbers are
    useful as `accumulators` (see the various examples in this
    class).  Functions are more convenient in expressions.

- number->equal: int|number|real
    Succeeds if the argument represents the same value.  Note that PCE type
    conversion converts real objects, number objects and text to ints and
    thus all the following succeed:

    	?- send(number(1), equal, 1).
    	?- send(number(1), equal, 1.0).
    	?- send(number(1), equal, number(1)).
    	?- send(number(1), equal, '1').

- number->larger_equal: int|number|real
    Test if larger-or-equal than argument.

- number->less_equal: int|number|real
    Test if less-or-equal than argument.

- number->not_equal: int|number|real
    Test if not-equal to argument.

- number->smaller: int|number|real
    Test if less than argument.

- number->larger: int|number|real
    Compare the receiver to the argument by value.  The receiver is tested
    to be {->larger, ->smaller, ...} than the argument.

- number->minimum: int|number
    Set value to largest of current and argument.

- number->maximum: int|number
    Set <-value to the {->maximum, ->minimum} of the current <-value and the
    argument.  Useful in may computations.  Suppose _Chain_ is a chain
    holding string and we which to know the widest string given the font
    _Font_:

    	width_of_strings(Chain, Font, Width) :-
    		new(W, number(0)),
    		send(Chain, for_all,
    			 message(W, maximum, ?(Font, width, @arg1))),
    		get(W, value, Width),
    		send(W, done).

## Get methods {#class-number-get}

- number<-catch_all: selector=name, argument=unchecked ... -> copy=number
    Makes all `send` operations available as get operations so that the
    receiving number is not modified:

    	?- new(N, number(2)),
    	   get(N, plus, 3, N2),
    	   get(N2, value, V2).

    	V2 = 5
    	N2 = @1687003/number
    	N = @1686999/number

    First creates a new number object with the same <-value, invokes the
    method as a send method and, if the send succeeds, returns the copy.

- number<-compare: int|number|real -> {smaller,equal,larger}
    Compare to the argument.  The result `smaller` implies the receiver is
    smaller than the argument.   Intended to support `chain ->sort` and
    friends.  To sort a chain of integers by value:

    	send(Chain, sort, ?(@arg1, compare, @arg2)).

