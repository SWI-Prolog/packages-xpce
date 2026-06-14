# class real {#class-real}

A real object represents a floating point number in PCE.  As PCE is
designed first of all for developing user interfaces, reals are rarely
used and therefore represented as true objects.  The behaviour of reals
is very similar to the behaviour of  class number.

The value of a real object is represented as a double-precision
float using the native floating point format of the machine.

@see class binary_expression


## Send methods {#class-real-send}

- real->minus: real
    Subtract argument from value.

- real->plus: real
    Add argument to value.

- real->times: real
    Multiply value by argument.

- real->divide: real
    Basic arithmetic operations.  Note that possible resulting floating
    point exceptions are not handled gracefully in the current
    implementation.

- real->equal: real
    Test if equal to argument.

- real->larger_equal: real
    Test if larger-or-equal than argument.

- real->less_equal: real
    Test if less-or-equal than argument.

- real->not_equal: real
    Test if not-equal to argument.

- real->smaller: real
    Test if less than argument.

- real->larger: real
    Compare the receiver's <-value to the <-value of the argument.  It
    ->larger succeeds, the receiver is larger than the argument.  Note
    that type conversion allows for the comparison of integers and
    numbers too.

- real->value: real
    Copy the <-value from the <-value of the argument.


## Get methods {#class-real-get}

- real<-catch_all: selector=name, argument=unchecked ... -> copy=real
    Non-destructive arithmetic.  Create a new real object with the same
    <-value, execute the operation and, if the operation succeeds, return
    the new real.   See also `number <-catch_all`.

- real<-value: -> real
    Equivalent to <-self.

    **Bugs**: It might be better to return a copy.

