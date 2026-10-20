# class gamepad_input {#class-gamepad_input}

A gamepad_input is the `event<-detail` of the gamepad_button_down,
gamepad_button_up and gamepad_axis events.  As the event delegates to
its detail, the values are normally requested from the event:

	get(Ev, control, Button)

@see class gamepad
@see event<-detail
@see class event_detail


## Instance variables {#class-gamepad_input-instvars}

- gamepad_input<-control: name
    The name of the button or axis.  See class gamepad.

- gamepad_input<-value: num
    1 or 0 for a button that is pressed or released, the position of
    the axis for gamepad_axis.


## Send methods {#class-gamepad_input-send}

- gamepad_input->initialise: control=name, value=num
    Create from the given values.
