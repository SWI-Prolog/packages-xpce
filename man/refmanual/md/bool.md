# class bool {#class-bool}

Class bool defines represents PCE's two boolean constants: @on and @off.
These objects are protected (see ->protect).  No other boolean constants
can be created.

@tbd Methods for logical expressions are still lacking.

@see class constant


## Send methods {#class-bool-send}

- bool->initialise
    Generates the error `!cannot_create_instances`.

- bool->unlink
    The two instances of class bool cannot be unlinked as they are
    ->protect'ed.


## Get methods {#class-bool-get}

- bool<-convert: any -> bool
    The following values are converted to booleans:

    | 0				| @off |
    | any other integer		| @on  |
    | '@on'			| @on  |
    | '@off'			| @off |
    | true, yes, `@on <-name`	| @on  |
    | false, no, `@off <-name`	| @off |

- bool<-negate: -> bool
    Maps @on to @off and visa versa.  The recogniser object below will
    toggle the `graphical ->inverted`:

	?- new(@toggle_inverted,
	       click_gesture(left, '', single,
			     message(@receiver, inverted,
				     @receiver?inverted?negate))).

