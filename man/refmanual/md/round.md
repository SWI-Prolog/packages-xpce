# class round {#class-round}

Arithmetic function.  _round_ rounds the value of its
`unary_expression-argument` to the nearest integer.  Halves are
rounded away from zero, as Prolog's `round/1`.

	?- new(B, box), send(B, width, round(7/2)), get(B, width, W).
	W = 4.

@see class unary_expression
@see class int_divide
