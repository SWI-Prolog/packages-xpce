# class tuple {#class-tuple}

Pair of arbitrary objects.  Tuples are most commonly used by methods to
return two values (PCE methods can only return a single value).  Methods
that have to return more than two values may return a chain object,
vector object or sheet object.


## Send methods {#class-tuple-send}

- tuple->initialise: first=any, second=any
    Create a tuple from its <-first and <-second element.

