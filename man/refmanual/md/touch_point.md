# class touch_point {#class-touch_point}

A touch_point is the `event<-detail` of the events of a finger on a
touch screen.  The first finger that touches the screen acts as the
left mouse button: it generates ms_left_down, ms_left_drag and
ms_left_up, so applications that only use the mouse also work with a
touch screen.  While it is down, other fingers generate touch_down,
touch_motion, touch_up and touch_cancel.  These are delivered to the
window of the first finger.  If the first finger is lifted while
others are still down, no finger acts as the mouse until all fingers
are lifted.  If the system cancels the first finger, e.g., because it
recognised a gesture, it generates ms_left_up.

The `event<-device` of these events is the input_device of the touch
screen, whose `input_device<-kind` is `touch`.  As the event delegates
to its detail, the values below are normally requested from the event:

	get(Ev, finger, Finger)

@see event<-detail
@see class input_device
@see class event_detail


## Instance variables {#class-touch_point-instvars}

- touch_point<-finger: int
    Identifies the finger while it touches the screen.

- touch_point<-pressure: num
    Pressure, from 0 to 1.

- touch_point<-dx: num
    Horizontal motion since the previous event of this finger, in
    pixels.

- touch_point<-dy: num
    Vertical motion since the previous event of this finger, in
    pixels.


## Send methods {#class-touch_point-send}

- touch_point->initialise: finger=int, pressure=[num], dx=[num], dy=[num]
    Create from the given values.  Omitted values are 0.
