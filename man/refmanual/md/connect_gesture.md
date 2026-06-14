# class connect_gesture {#class-connect_gesture}

A connect_gesture is used to create a connection object between two
graphical objects by dragging from the first to the second graphical
object.  The gesture is connected to a graphical that can serve as an
origin (starting) point for a connection.  Note that the graphical
should have handle objects that can connect to the link object
associated with the gesture.  Thus, if the link is created from
the term link(in, out), the graphical from which the connection
is started should have at least one handle of `handle<-kind`
`in` and the graphical at  the other end should have at least
one handle of `handle<-kind` `out`.

This class provides a high-level user-interface for creating
connections.  The following program illustrates the use of
PCE's graphical connection facilities:

	connect_demo :-
		send(new(P, picture('Connect Demo')), open),
		send(P, recogniser,
		     click_gesture(left, '', single,
						   message(@prolog,
								   display_box, P,
								   @event?position))).

	:- pce_global(@east,  new(handle(0, h/2, any, east))).
	:- pce_global(@north, new(handle(w/2, 0, any, north))).
	:- pce_global(@west,  new(handle(w, h/2, any, west))).
	:- pce_global(@south, new(handle(w/2, h, any, south))).

	:- pce_global(@connect_recogniser,
			      new(handler_group(connect_gesture(left, '',
								  			        link(any)),
				  					new(move_gesture)))).


	display_box(P, Pos) :-
		send(P, display, new(B, box(30,30)), Pos),
		send_list(B, handle,
				  [ @east, @north, @west, @south
				  ]),
		send(B, recogniser, @connect_recogniser).

Unfortunately possible links often rely on semantics too complicated to
deal with by the handle and link naming mechanism.  For this reason
this class defines a large number of methods that may be redefined.
The most commonly redefined are:

	| ->verify   | Additional tests, assign <-device |
	| <-pointed  | Determine graphical to link too   |
	| ->indicate | Find possible connection-points   |
	| ->connect  | Creates the actual connection     |

The default methods allow for connecting objects displayed on
the same device.  In principle, connect-gestures may be used
to connect any two objects displayed on the same window object,
arbitrary nested in device objects.  To use this feature, the
->verify method should be redefined and should set ->device
to the common device displaying both the origin and target
objects (this can be the window).  The <-pointed method should
be redefined to find graphicals under the pointer that can be
used to connect to.  The methods `device <-find` and `device
<-pointed' are often useful to implement the <-pointed.

See also class handle, class connection, class link, `graphical
->connect'.   The PceDraw demo illustrates the use of connections.

## User interface {#class-connect_gesture-user-interface}

On a *down* event (of the appropriate button with the appropriate
modifiers), the gesture will indicate the possible connection-points of
the graphical with the from-side of the link using small markers.  If
the down occurs close to one specific handle, the gesture assumes this
is the only handle  the user wants to connect to.  On termination, the
from-side of the connection will be fixed to this handle.  Otherwise all
handles of the appropriate kind are indicated.  In this case the
connection will not be fixed and moving or resizing one of the objects
may reconnect the connection to a visually more attractive handle.

On each subsequent *drag* event, the gesture will indicate valid
`to-handles` if they exist.

When the gesture is terminated after an *up* event, the appropriate
connection is created.

@see class connection


## Class variables {#class-connect_gesture-classvars}

- connect_gesture.mark: image = @mark_handle_image
    @see connect_gesture-mark


## Instance variables {#class-connect_gesture-instvars}

- connect_gesture<->device: device*
    The <-from_indicators and <-to_indicators are displayed on this device.
    Also, the recognisers looks for possible graphicals to connect too on
    this device.  May be changed in a redefined ->initiate to any device in
    the same window that displays the receiving graphical.

- connect_gesture<-from_handle: [name]
    Name of the handle at the `from` side that has to be used for the
    connection.  @default if there are more valid candidate handles,
    the name of the only candidate otherwise.  Set by ->initiate.

    @see connect_gesture->initiate

- connect_gesture<-from_indicators: chain
    Chain of bitmaps used to indicate the connection-points at the
    from-side.  Used internally.  These bitmaps are copies of the <-mark
    slot of the gesture.

    @see connect_gesture-mark

- connect_gesture<-line: line
    Line used for feedback during dragging.  It is a dotted line with
    otherwise the same characteristics as the line associated with <->link
    of the gesture.

- connect_gesture<->link: link*
    Link from which the gesture will create a connection on success.  If the
    user wants the gesture to get the link from the current context, the
    ->verify method should be redefined to set the appropriate link value.

    @see connect_gesture->verify

- connect_gesture<->mark: image
    Image object used to mark possible connection-points.  This image is
    used by the bitmaps in the <-from_indicators and <-to_indicators chains.
    See also ->indicated

    @see connect_gesture.mark
    @see connect_gesture-to_indicators
    @see connect_gesture-from_indicators

- connect_gesture<-to: graphical*
    Graphical to which the connection is made.  Set by ->drag and passed to
    ->connect by ->terminate.

- connect_gesture<-to_handle: [name]
    Name of the handle at the to-side if there is only one candidate.
    @default otherwise.

- connect_gesture<-to_indicators: chain
    Chain of bitmaps used to indicate the connection-points at the
    from-side.  Used internally.  These bitmaps are copies of the <-mark
    slot of the gesture.

    @see connect_gesture-mark


## Send methods {#class-connect_gesture-send}

- connect_gesture->connect: from=graphical, to=graphical, link=link, from_handle=[name], to_handle=[name]
    This method is called from ->terminate.  It's responsibility is to
    create the actual connection object.  The default implementation is to
    invoke `graphical ->connect` with the corresponding arguments.  This
    method may be redefined.

- connect_gesture->drag: event
    Updates the <-end of the <-line.  Next searches for graphicals on
    <-device that overlap with the pointer and have suitable handles.
    When found, invokes ->indicate to provide visual feedback to the
    user on the matching handles.

- connect_gesture->indicate: at=graphical, near=event, handle_kind=name, indicators=chain, variable={to_handle,from_handle}
    If there are any possible handle objects within 10 pixels of the
    pointer, indicate the closest using ->indicate_handle.and fill
    the given variable (<-to_handle or <-from_handle) with the name of the
    closest handle.

    Else if there are any pointer handle objects anywhere on the graphical
    under the pointer, indicate them.  Fill the given variable with
    @default.

- connect_gesture->indicate_handle: at=graphical, handle=name, indicators=chain
    Indicate the position of the named handle using a free indicator bitmap
    from the given list of indicators.  Whether or not an indicators is
    `free` is determined by the `bitmap <-name` of the indicator: `unused`
    or `used`.  Used by ->indicate.

- connect_gesture->initialise: button=[button_name], modifier=[modifier], link=[link]
    Create a connect_gesture object from the button it should operate on,
    possible modifier keys and the link object to use for the connection.
    This link object is used to determine the valid handle objects for the
    connection object to be created as well as the attributes of the <-line
    used for feedback.

- connect_gesture->initiate: event
    Set's up the connect gesture.  This consist of the following steps:

    1. Prepare the <-line using:

    		a) `line ->copy` using the line of the <-link.
    		b) `line ->texture: dotted`
    		c) `line ->points with the <-x and <-y of the event

    2. Display the line on <-device

    3. ->indicate the handle objects on the receiving graphicals.

    @see connect_gesture-from_handle

- connect_gesture->terminate: event
    Invokes ->drag.  Next removes the <-line and the indicator bitmaps
    from the <-device and finally invokes ->connect using the following
    arguments:

    	| `@event<-receiver` | The `from` graphical           |
    	| <-to               | The `to` graphical             |
    	| <-link             | The <-link                     |
    	| <-from_handle      | Handle name at the `from` side |
    	| <-to_handle        | Handle name at the `to` side   |

- connect_gesture->verify: event
    Fails if either the receiving graphical object has no `graphical
    <-device' or <-link equals @nil.

    Otherwise assigns <-device to the `graphical <-device` and succeed.

    @see connect_gesture-link


## Get methods {#class-connect_gesture-get}

- connect_gesture<-pointed: event -> chain
    Return a chain object holding candidate graphical objects to connect
    with.  The default method returns all graphicals overlapping
    with the event object on <-device, computed using `device
    <-pointed_objects'.  May be redefined.

