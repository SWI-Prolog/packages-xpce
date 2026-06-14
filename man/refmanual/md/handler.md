# class handler {#class-handler}

A handler object is a reusable object that defines how a graphical
object should respond when it is sent an event object.  A
handler maps an `event <-id` onto an action realised .by a code
object.

By default, handlers traps events anywhere on an object.  Using the
<-region, the handler can be active for a subregion of the object.

**Bugs**:

Handlers only handle single events and have no elegant way to represent
status information.  The are retained for backward compatibility
reasons.  New code should first look at the class gesture or
class key_binding.

@see @event_tree
@see class gesture
@see class region
@see topic Event types
@see class popup
@see object->recogniser


## Instance variables {#class-handler-instvars}

- handler<-event: event_id
    `event <-id` of the events this handler is sensitive to.   When an event
    arrives, it is tested using `event ->is_a`, given the value of this
    variable.

- handler<->message: code*
    Code executed when the handler `fires`.  Arguments:

    	| @receiver | `event <-master` (normally `event <-receiver`). |
    	| @arg1     | The event.                                      |

- handler<->region: [region]
    Region of graphical the event must be in.  See class region for details.
    Used infrequently.


## Send methods {#class-handler-send}

- handler->event: event
    Request to process an event.  This method is normally invoked from
    `graphical ->event`.  It will forward <-message iff `event ->is_a`
    succeeds for the argument event using <-event and if no <-region
    is specified or the argument event is inside the <-region.

- handler->initialise: event=event_id, message=code*, restrict_to=[region]
    Create a handler from the event_id mapped, the code it is mapped on and
    optionally a region to which its activation is limited.  For semantics,
    see ->event.

