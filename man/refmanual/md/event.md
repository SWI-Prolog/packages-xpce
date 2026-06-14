# class event {#class-event}

An event is PCE's notion of representing some activity from the user.
Such an activity may be the user pressing a key on the keyboard, a
button of the mouse, changing the mouse's location, etc.

Events are typed and their types are organised in a hierarchy.  See
Class event_tree.  Note that event-types have no relation to PCE
data-types as described by class type.

Events are trapped from the X-window environment by class window.  This
class deals internally with low-level events such as repaint requests.
All events that may be of interest to the user are transformed into PCE
event objects, which are then dispatched by a graphical device object.

There are several ways to program event handling.  The most
common way is to use gesture objects.  A gesture object parses
one or a sequence of events into meaningful actions such as
click, move, resize, connect, etc.   Gestures may be attached to
graphical objects using `graphical ->recogniser`.   When using
user-defined classes, events may be parsed by redefining the
`graphical ->event` method.  Gestures are normally used to
realise the event-parsing in the redefined method.

**Bugs**:

The single inheritance hierarchy for event type specification is not
very satisfactory.  A multiple inheritance hierarchy would be better.
A common problem arrising is that you may wish to have all left-button
events, regardless of whether it is an up, down or drag or all
button-down events, regardless of the exact button.

ASCII events are defined very ungracefully.  Be aware that consistency
with class editor will probably be implemented some day, changing --for
example-- type _3_ into _\C-c_.

@see class gesture
@see class recogniser
@see class event_tree
@see topic Events
@see class dialog_item
@see class window
@see class device
@see object->recogniser


## Class variables {#class-event-classvars}

## Instance variables {#class-event-instvars}

- event<-buttons: mask=int
    Bitmask containing various status information on the event.  It is not
    advised to rely on the contents of the bitmask as this might change in
    the future.

    The status of the modifier keys is normally tested using ->has_modifier.
    The type of click (single, double, triple) is requested using
    <-multiclick and the button for button-related events using <-button.

    Current definition of the mask-values

    	control-key		0x1
    	shift-key		0x2
    	meta-key		0x4
    	left-button		0x8
    	middle-button	0x10
    	right-button	0x20

    	single-click	0x40
    	double-click	0x80
    	triple-click	0xC0

- event<-id: event_id
    Identifier for the event.  This is either a name occurring in the
    event-type hierarchy @event_tree or an integer.  The latter is used for
    ASCII-type events and represents the ASCII value.  See also
    <-key.

    Whether or not an event is of specified type is normally tested using
    ->is_a.  Other useful classifications are realised using
    ->is_up, ->is_down, ->is_drag, ->has_modifier, <-click_time and
    <-click_displacement.

    One event requires special treatment.  This is the event related
    to `wheel-mice`, mice normally having two buttons and a
    scroll-wheel.  Rotations of the scroll-wheel are mapped to an
    event object with <-id: wheel.  Conform the Microsoft windows
    conventions, this event is has a `rotation` attribute,
    associate using `->attribute`.  The rotation has the value 120
    if the wheel is rotated from the user and -120 when rotated
    towards the user.

    In the X11 version, wheel-events are normally mapped to
    the virtual mouse-buttons 4 and 5.  If the class-variable
    event.x11_wheel_mouse = @on (default in the X11 version),
    the low-level event handling will map the button 4 and button 5
    events to `wheel` events.

    The wheel event is handled by class window, class editor,
    class list_browser and class scroll_bar.

    @see event->is_a

- event-position: point*
    This slot is initially @nil.  It is filled when the event position is
    computed through <-position or <-area_position.  Subsequent calls to one
    of these methods will update the `point <-x` and `point <-y`
    values and return the same point object.

    @see event<-position
    @see event<-area_position

- event<-receiver: graphical|frame
    Object the graphical is currently posted to by `event ->post`.  Note
    that this graphical is not necessarily the object that initialy
    received the event.   See also <-window.

    @see event-window
    @see event->post

- event-time: alien:Time
    Timestamp of the event in milliseconds since this PCE process was
    initialised.  Used infrequently.  See also <-multiclick.

- event<-window: window|frame
    Window from which the event was generated.  See also `window ->event`.

    @see event-y
    @see event-x
    @see event-receiver

- event<-x: pixels=int
    X-coordinate of the event relative to <-window.  Note that this position
    is *not* in the window's coordinate system, but relative to the top-left
    corner of the X-widget realising the window.

    @see event-window
    @see event-y

- event<-y: pixels=int
    Y-coordinate of the event relative to <-window.   See <-x for details.

    @see event-window
    @see event-x


## Send methods {#class-event-send}

- event->has_modifier: modifier
    Succeeds if the event satisfies the condition of the argument modifier
    object.

    A modifier may be created by conversion from a name consisting of the
    letters 's' (shift), 'c' (control) and 'm' (meta).  The empty name
    demands all modifier keys to be up.  See `modifier <-convert` for
    details.

    The following tests whether the shift key of the current event is
    depressed (and the control- and meta-keys are *not* depressed).

    	send(@event, has_modifier, s).

    @see modifier<-convert
    @see class modifier

- event->initialise: id=event_id, origin=[window|frame], x=[int], y=[int], button_mask=[int], time=[int]
    Create an event from the specified parameters.  Events are
    seldomly generated explicitly by the application code.  All
    optional arguments default to their value of the last processed
    event.  See also `graphical ->generate_event`.

    @see graphical->generate_event

- event->inside: [graphical]
    Succeeds if event is in the event-sensitive area of the argument
    graphical (see `graphical ->in_event_area`).

    If the argument is a window it checks whether the event is in
    the client area of the window.

    **Defaults**: The default graphical is the <-receiver.

    @see area->in
    @see graphical->in_event_area

- event->is_a: super=event_id
    Succeed if the event matches the specifier.  The event-specifiers are
    organised in a hierarchy, represented by @event_tree.  This method
    succeeds if <-id is at or below the specified node in @event_hierarchy.

    This test is the common way to test the identity of events.  It is used
    by handler objects.  Note that class gesture provides a more advanced
    mechanism to deal with mouse events.

    ASCII events (<-id is an integer) are organised as follows:

    	ascii
    	    control
    	        0..31|127
    	    printable
    	        32..126
    	    meta
    	        128..255

    The demo tool _Events_ displays @event-hierarchy.

    @see event-id
    @see class event_tree
    @see object->recogniser

- event->is_down
    Succeeds if the event describes a button-down of any of the existing
    mouse-buttons.

    @see event->is_up

- event->is_drag
    Test if event is a button-drag event.  A drag event is a
    `loc_move` event with one of the pointer-buttons depressed.

- event->is_up
    Succeeds if the event represents a release of one of the mouse-buttons.
    See also ->is_down.

    @see event->is_down

- event->post: to=graphical, recogniser=[recogniser]
    This method is the central function for delivering events to graphical
    objects.  This method takes care of automatically grabbing the
    event-focus on mouse-down events.  See also class gesture.

    It may also be used to forward events to other graphicals.  It performs
    the following steps:

    - Bind @event to the receiving event object

    - Bind <-receiver to the argument graphical

    - If the recogniser is @default, invoke `graphical ->event` to
    	the argument graphical.  Otherwise invoke `recogniser ->event`
    	on the argument recogniser.  The latter mechanism is used when
    	a `window ->event` posts an event to its focus and
    	`window <-focus_recogniser` is not @nil.

    - If the event, nor the receiver of the event is freed and the
    	event is accepted and

    - The <-window has no `window <-focus` and
    - The event is a ->is_down event and
    - graphical is displayed on `event <-window`

    	invoke `window ->focus: graphical, @nil, @default, <-button`

    @see event-receiver
    @see @event
    @see graphical->generate_event
    @see window->event
    @see device->event
    @see window->focus


## Get methods {#class-event-get}

- event<-area_position: relative_to=[graphical] -> point
    Returns the location of the event relative to the `area <-x` and `area	<-y`
    of the argument graphical.

    Note that both <-position and <-area_position reuse the point object in
    the slot -position.

    **Defaults**: The default graphical is the <-receiver of the event.

    @see event-position
    @see event<-position

- event<-button: -> button_name
    Returns the name of the button to which the event is associated.  The
    predefined button-names are left, middle and right.  Fails with a
    warning if the event was not associated to a button.

- event<-click_displacement: -> pixels=int
    Number of pixels the pointer has moved since the last down
    event.  See also <-click_time, ->is_up, <-multiclick and class
    click_gesture.

- event<-click_time: -> milliseconds=int
    Returns the time in milliseconds of this event object since the
    last `down` event.  See also <-click_displacement,
    <-multiclick, ->is_up and class click_gesture.

- event<-convert: [any] -> event
    Converts the constant @default into the var object @event.  This
    conversion function allow us to omit specifying the event if this is the
    current event.

- event<-display: -> display
    Display object on which the event occurred.  When using only a single
    display, this will be @display.  Computed using `window <-display` on
    the associated <-window.

- event<-distance: to=event -> int
    Rounded integer distance between events.  Both events must be
    in the same <-window.

- event<-inside_sub_window: [display|frame|window] -> frame|window
    This method may be used to relate an event to an XPCE window.
    Normally `event<-window` refers to the window in which the event
    occurred.  If a window has the event focus however,
    `event<-window` returns the window that has the focus rather
    than the window where the pointer is.

    	get(Event, inside_sub_window, Frame)

    will return the XPCE frame in which the event occurred.

    	get(Event, inside_sub_window, Frame, Window)

    will return the window in which the event occurred, where Frame
    is the return value of the previous call.  This call returns the
    actual window (*not* the window_decorator object) if the event
    occurred in a normal window.  If the event occurred in a
    decoration (scroll_bar object, window-label) the window_decorator
    is returned.

    This method is used by the Prolog defined class
    drag_and_drop_gesture to locate the target for the drop
    operation which may be in another window.  See
    $PCEHOME/prolog/lib/pce_drag_and_drop.pl.

- event<-key: -> name
    Key(-binding) description of event.  If the event is not a
    keyboard event, this is the same as <-id.  Otherwise it
    is a symbolic description of the depressed key.  The
    key-description is described with `key_binding ->function`.

- event<-master: -> any
    The `master` of an event is the `master` of its <-receiver.  See `visual
    <-master' for details.

- event<-multiclick: -> {single,double,triple}
    Detect multiclick.  It yields one of the names single, double or
    triple.  Multiclick detection is based on the time interval between
    this button-down event and the previous and the distance between these
    events.

    Clicking is normally handled using a click_gesture object.

    NOTE:	When there are both recognisers for single- and double-click,
    - both* will be executed on a double click!

    **Defaults**:
    A click is regarded as the `next step` in a multiclick sequence if it
    the down event is within 400 milliseconds of previous down event and
    within 4 pixels displaced.

    **Bugs**: These methods should read the corresponding X-resources.

    @see class click_gesture

- event<-name: -> event_id
    Synonym for <-id.  Backwards compatibility only.

- event<-x: relative_to=[graphical|frame|display] -> int

- event<-y: relative_to=[graphical|frame|display] -> int

- event<-position: relative_to=[graphical|frame|display] -> point
    Position of the event relative to the given object.  The argument
    may be any graphical object, frame object or the display provided it is
    displayed on the same display object as where the event occurred.

    For device objects (and windows), the returned position is relative to
    the origin (`device <-offset`) of the device.  For other graphicals it
    is relative to the `area <-x` and `area <-y` of the graphical's area.
    For a display object it is relative to the top-left corner of the
    display.  See also <-area_position.

    The returned value is also stored in the -position slot.

    @see event-position
    @see event<-area_position
    @see point<-convert
