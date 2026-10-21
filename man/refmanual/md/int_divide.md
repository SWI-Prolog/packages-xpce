# class int_divide {#class-int_divide}

Arithmetic expression.  _//_ evaluates both its `binary_expression-left`
and `binary_expression-right` operands and returns their integer
quotient, truncated towards zero as Prolog's `//`.  Unlike `/`, the
result is an integer if the operands are not.  A zero divisor raises
the error `division_by_zero`.

	?- get(7//2, value, X).             ==> X = 3
	?- get(-7//2, value, X).            ==> X = -3

@see class divide
@see class round
@see class floor
