# class pen_state {#class-pen-state}

A pen_state is the `event<-detail` of the events of a pen on a graphics
tablet or touch screen.  The tip of the pen generates the ms_left_*
events, its first and second barrel button the ms_right_* and
ms_middle_* events and moving the pen while it is near the surface
generates loc_move events, so applications that only use the mouse
also work with a pen.  The `event<-device` of these events is the
input_device of the pen, whose `input_device<-kind` is `pen`.  In
addition, a pen generates pen_proximity_in and pen_proximity_out when
it comes near or leaves the surface and pen_axis if an axis changes
without moving the pen.

The pen_state holds the state of all axes of the pen when the event was
generated.  As the event delegates to its detail, these are normally
requested from the event, e.g.

	get(Ev, pressure, Pressure)

Axes that the pen does not have are 0.  The names follow SDL, except
for <-hover_distance and <-barrel_rotation, as an event already has a
<-distance and a <-rotation.

@see event<-detail
@see class input_device
@see class event_detail


## Instance variables {#class-pen-state-instvars}

- pen_state<-pressure: num
    Pressure of the tip, from 0 to 1.

- pen_state<-xtilt: num
    Horizontal tilt, from -90 to 90 degrees.  Positive tilts to the
    right.

- pen_state<-ytilt: num
    Vertical tilt, from -90 to 90 degrees.  Positive tilts down.

- pen_state<-hover_distance: num
    Distance of the tip to the surface, from 0 to 1.

- pen_state<-barrel_rotation: num
    Rotation of the barrel, from -180 to 180 degrees, clockwise.  0 is
    facing up.

- pen_state<-slider: num
    Position of the finger wheel or slider of, e.g., an airbrush pen,
    from 0 to 1.

- pen_state<-tangential_pressure: num
    Pressure from squeezing the barrel, from 0 to 1.

- pen_state<-eraser: bool
    @on if the eraser end of the pen is used.

- pen_state<-barrel_buttons: int
    Bit mask of the barrel buttons that are down.  1 is the first
    button, 2 the second, etc.


## Send methods {#class-pen-state-send}

- pen_state->initialise: pressure=[num], xtilt=[num], ytilt=[num], hover_distance=[num], barrel_rotation=[num], slider=[num], tangential_pressure=[num], eraser=[bool], barrel_buttons=[int]
    Create from the given values.  Omitted values are 0 or @off.
