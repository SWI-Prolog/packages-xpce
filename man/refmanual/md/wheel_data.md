# class wheel_data {#class-wheel-data}

A wheel_data is the `event<-detail` of the wheel and horizontal_wheel
events.  `event<-rotation` reports whole notches in the dominant
direction.  A wheel_data holds the precise scroll distance in both
directions since the previous wheel event, as reported by trackpads
and high resolution wheels.  As the event delegates to its detail, the
methods below are normally called on the event:

	get(Ev, dy, DY)

The distances use the same sign as `event<-rotation` and the unit is a
notch: positive `dy` scrolls up (the wheel is rotated away from the
user) and positive `dx` scrolls right.

@see event<-detail
@see event-rotation
@see class event_detail


## Instance variables {#class-wheel-data-instvars}

- wheel_data<-dx: num
    Horizontal scroll distance in notches.  Positive is to the right.

- wheel_data<-dy: num
    Vertical scroll distance in notches.  Positive is up.

- wheel_data<-flipped: bool
    @on if the system uses "natural" scrolling, where the content
    follows the fingers.  <-dx and <-dy are as reported by the system;
    negate them to obtain the physical direction of the movement.


## Send methods {#class-wheel-data-send}

- wheel_data->initialise: dx=num, dy=num, flipped=[bool]
    Create from the given distances.  `flipped` defaults to @off.
