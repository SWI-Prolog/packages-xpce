# class keyboard {#class-keyboard}

The keyboard is the input_device @keyboard_device.  Keyboard events
refer to it as their `event<-device`.  The window system combines all
keyboards, so there is a single keyboard object.

Besides delivering events, the keyboard can be polled using
<-key_down.  This is needed by applications such as games that act
on keys being held, as there are no events for releasing a key.  A
game typically reads the keys on each tick of a timer:

	tick(Ship) :-
	    (   get(@keyboard_device, key_down, cursor_left, @on)
	    ->  send(Ship, rotate, -5)
	    ;   true
	    ),
	    ...

@see class input_device
@see class gamepad


## Get methods {#class-keyboard-get}

- keyboard<-key_down: key=name -> bool
    `@on` if the key is held down.  `key` is the name of a key event,
    e.g., `cursor_left`, `RET`, `ESC` or `f1`, a single character,
    `space`, or the name SDL uses for the key, e.g., `Left Shift`.  A
    character refers to the key that produces it in the current
    keyboard layout.  Fails with a report if the key is unknown.
