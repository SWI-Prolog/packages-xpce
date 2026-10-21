# class floor {#class-floor}

Arithmetic function.  _floor_ rounds the value of its
`unary_expression-argument` down to an integer, as Prolog's
`floor/1`.

	?- new(B, box), send(B, width, floor(7/2)), get(B, width, W).
	W = 3.

@see class unary_expression
@see class int_divide
