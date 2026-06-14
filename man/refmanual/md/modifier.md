# class modifier {#class-modifier}

A modifier object describes a condition on the modifier keys (SHIFT,
CONTROL and META).  Modifier objects are used to describe the modifier
condition of gestures or to validate the modifier condition for some
event.

Modifiers are normally not specified as an object, but as a name that is
automatically converted. In

	?- new(X, move_gesture(middle, c)).

`c` is automatically <-convert'ed into a modifier object.

@see gesture.modifier
@see gesture-modifier
@see class gesture
@see event->has_modifier


## Instance variables {#class-modifier-instvars}

- modifier<->control: [{up,down}]
    Condition on the key:

    - up
    	Key may not be depressed

    - down
    	Key must be depressed

    - @default
    	State of key is ignored

- modifier<->meta: [{up,down}]
    *Inherits description from*: modifier-control

- modifier<->shift: [{up,down}]
    *Inherits description from*: modifier-control


## Send methods {#class-modifier-send}

- modifier->initialise: shift=[{up,down}], control=[{up,down}], meta=[{up,down}], gui=[{up,down}]
    Create from the constraints on the shift-, control- and meta-keys.
    Usually, modifiers are specified using a string consisting of the
    characters 's', 'c' and 'm'.  See <-convert.

    @see modifier<-convert


## Get methods {#class-modifier-get}

- modifier<-convert: name -> modifier
    Convert a name, consisting of the characters `s` (shift), `c` (control)
    and `m` (meta) to a modifier object that demands the specified keys to
    be depressed and the others not to be depressed.

    The empty atom (''), transforms into a modifier that demands all keys
    not to be depressed.

    This is the normal way to specify modifiers.  For example, the following
    defines a click_gesture for handling shift-click:

    	new(G, click_gesture(left, 's', single,
    						 message(@event?receiver,
    						 		 selected, @on)).

    Converted modifiers are stored in the hash-table @modifiers for
    better performance and to exploit reuse.   As a consequence,
    modifiers created this way should by treated *read-only*.

    @see event->has_modifier
    @see modifier->initialise
    @see gesture.modifier

