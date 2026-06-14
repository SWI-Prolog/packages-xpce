# class int_item {#class-int_item}

Class int_item defines a specilised version of class text_item
for editing integer values.  The refinement defines the
following additional behaviour:

- Type checking
	Both during typing and setting/feching the selection,
	the value is verified to be an integer.

- Automatic setting of ->length
	The widfth of the field is automatically adjusted to
	fit all integers in the defined range

- increment/decrement buttons
	These buttons are shown to the right of the entry field.
	Pressing ->increment's or ->decrement's the <-selection
	and executes <-message.  If the mouse-button is held
	down, the increment/decrement repeats.  The upper and
	lower bounds of the ->range are honoured.

See also class text_item, class slider, and ->initialise.


## Instance variables {#class-int_item-instvars}

- int_item<-status: {inactive,active,preview,execute,increment,decrement}
    Status for event-processing.  The values `increment` and
    `decrement` are used for repeating.


## Send methods {#class-int_item-send}

- int_item->increment
    Increment the selection.

- int_item->decrement
    Decrement the selection.  This methods translates the currently
    displayed text into an integer (0 if the value cannot be
    translated), decrements the result by one and ensures the result
    is within the boundaries determined by ->range.

- int_item->initialise: name=[name], default=[function|int], message=[code]*, low=[int], high=[int]
    Create an int_item object from its name, default value, message
    and low and high value.  The initialisation is the same as
    `text_item ->initialise`, followed by ->range: low, high.

- int_item->range: low=[int], high=[int]
    Allowed range.  This methods assigns <-type and ->length to
    ensure the item is wide enough to hold all values in the
    indicated range.  The default `low` value is `@pce <-min_integer`,
    the default `high` is `@pce <-max_integer`.

- int_item->typed: event_id
    Invokes `text_item ->typed`.  If the result is not an integer,
    or not within ->range a warning is raised and the value is
    reset to the value before calling this method.

