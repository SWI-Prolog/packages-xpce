# class recogniser {#class-recogniser}

Class recogniser is an almost empty super-class.  It's main purpose is
to serve as a type-check for it's sub-classes.

A recogniser is an object that maps a user event onto an action.  The
events origins from the window on which it occurred and is dispatched by
the dispatching behaviour defined by `device ->event`.

A recogniser may be attached to a graphical object using `graphical
->recogniser' or it may be used from a redefined `graphical ->event`
method.   Multiple recognisers may be combined to a single entity using
class handler_group.

@see graphical->event
@see class event
@see topic Handling Events
@see object->recogniser
@see class interceptor
@see object<-all_recognisers
@see object->prepend_recogniser


## Instance variables {#class-recogniser-instvars}

- recogniser<->active: bool
    Recognisers can be made inactive using ->active: @off.  The method
    ->event fails for inactive recognisers.


## Send methods {#class-recogniser-send}

- recogniser->event: event
    Process an event.  The method defined at the level of class recogniser
    itself simply fails.

- recogniser->initialise
    Create a recogniser object.  Initialises ->active to @on.  Creating
    instances of this class is no common use.

