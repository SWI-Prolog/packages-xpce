# class move_outline_gesture {#class-move_outline_gesture}

A `move_outline` gesture is very similar to a move_gesture object,
except that it drags a dotted box instead of the object itself and only
moves the object when the user releases the button.

The move-outline gesture is to be preferred if the graphical has
constraints or connections because the move_gesture will force
recomputing these constraints and connections on each drag-event, while
the move_outline_gesture will do so only when the user releases the
button.

**Bugs**:

The outline is a box that is actually (temporary) displayed on the same
device as the graphical to be moved.  The current implementation of
PCE's repaint algorithm requires significant repaint operations to be
performed each time the outline-box is moved.  A dedicated
implementation of the outline (using XOR'ing or copying) would improve
efficiency.


## Instance variables {#class-move_outline_gesture-instvars}

- move_outline_gesture<-outline: box
    Box used to paint the outline moved.   The attributes `box ->texture`,
    `box ->pen`, etc, may be changed.


## Send methods {#class-move_outline_gesture-send}

- move_outline_gesture->drag: event
    Delegates the event to the <-outline using `event ->post`, thus moving
    the <-outline rather than the object.

- move_outline_gesture->initiate: event
    Displays the <-outline on the same device as the graphical moved.  Than
    uses `event ->post` to initiate the normal move_gesture object
    associated with the <-outline.

- move_outline_gesture->terminate: event
    First invoke ->drag.  Next it invokes ->set with the x, y, w and h of
    the area of the <-outline to `event <-receiver`.  Finally it invokes
    `graphical ->device: @nil` to the <-outline to remove it from the
    device.

