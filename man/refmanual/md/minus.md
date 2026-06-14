# class minus {#class-minus}

Arithmetic expression.  _-_ will evaluate both its left-hand and
right-hand side and return the difference between them.

**Bugs**: No checking on arithmetic overflow is implemented.


## Send methods {#class-minus-send}

- minus->initialise
    If only 1 argument is given, this is translated into -(0, X) to achieve
    a unary minus.

