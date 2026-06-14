# class nameref {#class-nameref}

The code class @= assigns a global named reference to an object.
It is introduced to provide for a better syntax to create global
objects from the XPCE Defaults file (notably display.initialise).

@= is defined as an infix operator (xfx, priority 990) by the
Defaults value parser.   The following example defines a global
object:

	_window_pen @= number(1)

After this the global object @_window_pen refers to a number
object with value 1.  See also the XPCE Defaults file, normally
located in $PCEHOME/Defaults, where $PCEHOME refers to
the XPCE homd directory (see `@pce <-home`).

The construct

	name @= object

is equivalent to

	message(object, name_reference, name).

Except that --if the object is a function object-- the function
is assigned the reference instead of the object the function
evaluates to.

See also `object ->name_reference`,  pce_global/2, class var
and class assign.

NOTE: Application programmers should *not* use this class.
it is introduced to allow for parsing the @= construct by
the Defaults parser.  Later releases of the parser may use an
alternative implementation (i.e.  some form of active
macro-expansion instead of executable objects).


## Instance variables {#class-nameref-instvars}

- nameref-object
    Object or function that should be assigned a reference name on
    ->execute'ing.

- nameref-reference
    Reference name given to <-object on ->execute.


## Send methods {#class-nameref-send}

- nameref->_execute
    Invoke send(<-object, name_reference, <-reference).  See also
    `object  ->name_reference`.

- nameref->initialise
    Simply fills <-reference  and <-object from the arguments.

