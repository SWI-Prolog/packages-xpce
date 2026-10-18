# class gamepad {#class-gamepad}

A gamepad is the input_device of a game controller.  Gamepads are
enabled by `display<-gamepads`, which also returns the connected
gamepads.  The buttons and axes of a gamepad generate events for the
window with the keyboard focus, as keys do:

  - gamepad_button_down, gamepad_button_up
    A button is pressed or released.
  - gamepad_axis
    A stick or trigger moved.

These events are `event->is_a: gamepad`.  Their `event<-detail` is a
gamepad_input, so `event<-control` gives the name of the button or
axis and `event<-value` its value.  Gamepad events are only generated
while the application has the keyboard focus.  When a gamepad is
connected or disconnected, the window with the keyboard focus of each
frame receives a device_added or device_removed event.

The buttons and axes are named after their position rather than their
label, so the names are the same for controllers of different brands:

  - Face buttons: `south` (A on an Xbox controller, cross on a
    PlayStation controller), `east`, `west` and `north`.
  - `back`, `guide`, `start`, `left_stick`, `right_stick`,
    `left_shoulder`, `right_shoulder`, `dpad_up`, `dpad_down`,
    `dpad_left`, `dpad_right`, `misc1` .. `misc6`, `left_paddle1`,
    `left_paddle2`, `right_paddle1`, `right_paddle2` and `touchpad`.
  - Axes: `left_x`, `left_y`, `right_x`, `right_y` (-1 .. 1; positive
    is right and down) and `left_trigger`, `right_trigger` (0 .. 1).

For example:

	event(W, Ev:event) :->
	    (   send(Ev, is_a, gamepad_button_down),
	        get(Ev, control, south)
	    ->  send(W, fire)
	    ;   send_super(W, event, Ev)
	    ).

@see display<-gamepads
@see class gamepad_input
@see class input_device


## Send methods {#class-gamepad-send}

- gamepad->initialise: kind={mouse,pen,touch,gamepad,keyboard}, id=[int], name=[name]
    Gamepads are created by xpce.

- gamepad->rumble: low=0.0..1.0, high=0.0..1.0, milliseconds=int
    Rumble the low and high frequency motors with the given strength
    for the given time.  Fails if the gamepad cannot rumble.

- gamepad->rumble_triggers: left=0.0..1.0, right=0.0..1.0, milliseconds=int
    As ->rumble, for the motors in the triggers.

- gamepad->led: colour
    Set the colour of the LED.  Fails if the gamepad has no LED.


## Get methods {#class-gamepad-get}

- gamepad<-connected: -> bool
    `@off` if the gamepad was disconnected.

- gamepad<-button: name -> bool
    `@on` if the named button is down.

- gamepad<-axis: name -> num
    The position of the named axis.

- gamepad<-type: -> name
    The type of the controller, e.g., `xbox360`, `ps5`, or `unknown`.

- gamepad<-player_index: -> int
    The player index of the gamepad.  Fails if it has none.
