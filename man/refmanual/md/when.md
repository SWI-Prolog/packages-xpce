# class when {#class-when}

When executed, a when object tests the condition and then evaluates
either the then- or else-function, returning its result.  This construct
is similar to C and similar languages ``Condition ? Then : Else``.

The example below illustrates how a method `number <-absolute` could be
attached to class number to return its absolute value:

	?- send(class(number), get_method,
	        get_method(absolute, int, new(vector),
			   when(@receiver < 0,
				-(@receiver),
				@receiver?value))).

	?- new(N, number(-4)),
	   get(N, absolute, V).		==> V = 4.

@see class if


## Instance variables {#class-when-instvars}

- when<->condition: code
    Condition object that determines whether the <-then or <-else branch is evaluated.

- when<->else: any|function
    Either of <-then or <-else is evaluated depending on the result of
    testing <-condition.  If this slot contains a function, this function is
    evaluated and the result returned.  Otherwise the plain value is
    returned.

- when<->then: any|function
    *Inherits description from*: when-else

## Send methods {#class-when-send}

- when->initialise: condition=code, then=any|function, else=any|function
  Create a when object.  The branches are either a concrete value or a function object.
