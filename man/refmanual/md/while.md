# class while {#class-while}

An instance of class while executes it's <-body until it's <-condition
fails, similar to the Pascal or C while statement.

While loops are rarely used in XPCE.  Most loops in XPCE entails
enumerating the elements of a collection.  For this purpose the
methods `chain ->for_all`, `vector ->for_all`, etc.  are more
appropriate.   The following two examples both print all elements
of Chain to the standard output:

	new(N, number(0)),
	get(Chain, size, Max),
	send(while(N < Max,
		   and(message(@pce, format, '%N\n',
			       ?(Chain, nth0, N)),
		       message(N, plus, 1))),
	     forward)

Or

	send(Chain, for_all,
	     message(@pce, format, '%N\n', @arg1))

## Instance variables {#class-while-instvars}

- while<->body: code*
  The body (statement).  If @nil, the condition is executed until it fails.

- while<->condition: code
  The condition.  Note that an _always true_ statement can be created as
  `new(and)` and an always false as `new(or)`.

## Send methods {#class-while-send}

- while->initialise: condition=code, statement=code*
    Create from condition and statement (<-body).

- while->_execute
    Execute the <-body while the <-condition holds.

@see topic Enumerate

