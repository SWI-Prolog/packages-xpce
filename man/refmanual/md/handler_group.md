# class handler_group {#class-handler_group}

A handler_group object is a collection of recogniser objects (hence, a
better name would be `recogniser_group`; this might change).  It
provides a means to combine a set of recognisers, each dealing with some
detailed UI behaviour into one object dealing with many UI behaviours.
Handler groups may be nested.  This process may be repeated to
get more complex event-recognisers.

It is common practice to combine recognisers.  Suppose the user
interface contains a (large) number of icons that should be
movable and double-click should `open` the icon..   In this case
a recogniser would be declared as:

	:- pce_global(@icon_recogniser, new(handler_group(
			new(move_gesture),
			new(click_gesture(left, '', double,
							  message(@receiver, open)))))).

**Bugs**: Should be called `recogniser_group`

@see object->recogniser


## Send methods {#class-handler_group-send}

- handler_group->delete: recogniser
    Delete first occurrence of recogniser

- handler_group->event: event
    Process an event object.  Simply invokes ->event on each of the
    <-members until this message succeeds.  If no recogniser accepts
    the event it fails.


## Get methods {#class-handler_group-get}

- handler_group<-_arg: int -> recogniser
    Return the nth element of <-members.  See `object <-_arg`.

- handler_group<-_arity: -> int
    Number of elements in <-members.  See `object <-_arity`.

