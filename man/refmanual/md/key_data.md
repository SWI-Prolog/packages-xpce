# class key_data {#class-key-data}

A key_data is the `event<-detail` of the events of the keyboard.  It
holds whether a key-down is auto-repeated and the key that a key_up
event releases.

Releasing a key generates an event with <-id `key_up`.  This event is
not a keyboard event (`event->is_a: keyboard` fails), so key bindings,
text items, editors and other code that handles keys do not see it.
Code that wants releases asks for them:

	event(W, Ev:event) :->
	    (   send(Ev, is_a, key_up)
	    ->  get(Ev, key, Key),        % Key is released
	        ...
	    ;   send(Ev, is_a, keyboard)
	    ->  get(Ev, key, Key),        % Key is pressed
	        ...
	    ;   send_super(W, event, Ev)
	    ).

`event<-key` of a key_up event is `event<-key` of the key-down it
releases, also if the modifiers changed in between: releasing Shift
before `a` after typing `A` releases `A`.  A key_up goes to the
keyboard focus, as the key-down.  Only keys that generated a keyboard
event generate a key_up: a modifier key, such as Shift, generates
neither.  A key_up may arrive without a key-down event at the same
receiver if the keyboard focus moved while the key was held.

@see event<-key
@see event<-repeat
@see keyboard<-key_down
@see class event_detail


## Instance variables {#class-key-data-instvars}

- key_data<-key: name
    `event<-key` of the key-down: the character typed, e.g., `a` or
    `A`, or the name of the key, e.g., `RET` or `cursor_left`.

- key_data<-repeat: bool
    `@on` if the key-down is auto-repeated because the key is held.
    @off for a key_up.


## Send methods {#class-key-data-send}

- key_data->initialise: key=name, repeat=[bool]
    Create from the given key.  `repeat` defaults to @off.
