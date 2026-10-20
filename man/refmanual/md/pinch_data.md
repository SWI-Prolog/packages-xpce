# class pinch_data {#class-pinch_data}

A pinch_data is the `event<-detail` of the pinch_begin, pinch_update
and pinch_end events, which are generated for the zoom gesture of a
trackpad: moving two fingers apart or together.  These events are
mouse events (`event->is_a: pinch` and `event->is_a: mouse`) of
@mouse_device at the position of the mouse.  As the event delegates to
its detail, the scale is normally requested from the event:

	get(Ev, scale, Scale)

The recogniser of library(pan_zoom) zooms a figure on a pinch.

@see event<-detail
@see class event_detail


## Instance variables {#class-pinch_data-instvars}

- pinch_data<-scale: num
    Change of the scale since the previous pinch_update.  A scale
    above 1 zooms in, below 1 zooms out.  The scale of pinch_begin
    and pinch_end is 1.


## Send methods {#class-pinch_data-send}

- pinch_data->initialise: scale=[num]
    Create with the given scale, which defaults to 1.
