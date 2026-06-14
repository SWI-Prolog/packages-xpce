# class move_gesture {#class-move_gesture}

A `move_gesture` defines the behaviour necessary to allow the user
dragging a graphical object with the mouse over it's graphical device.

While dragging, the cursor is changed to a `fleur`

By default, the move_gesture is associated with the middle mouse-button.


## Instance variables {#class-move_gesture-instvars}

- move_gesture<-offset: point
    While active, the distance between the top-left corner of the graphical
    moved and the cursor.  Set by ->initiate.


## Send methods {#class-move_gesture-send}

- move_gesture->drag: event
    Moves the graphical using the ->do_set behaviour so that the
    distance between the cursor and the top-left remains the same as
    when the gesture was initiated.

- move_gesture->initialise: button=[button_name], modifier=[modifier]
    Create a move_gesture, associated with a button with modifiers.  Both
    the button and the modifiers are determined by their
    class-variables.  One should avoid to use explicit binding of a
    move gesture to some button.

    **Defaults**:
    	button	Resource defined (middle)
    	modifiers	Resource defined (none)

- move_gesture->initiate: event
    Fills <-offset with the distance of the mouse-down event to the top-left
    corner of the graphical on which the move is initiated.  Sets the cursor
    to move_gesture.cursor.

- move_gesture->terminate: event
    Equivalent to ->drag.

- move_gesture->verify: event
    Succeeds if the receiving graphical object has <->device not equal to
    @nil.

