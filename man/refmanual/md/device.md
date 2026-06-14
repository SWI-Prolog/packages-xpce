# class device {#class-device}

A (graphical) device is a collection of graphical objects.  As device
itself is a graphical objects; devices may be nested.  Class device
describes the management of such a collection: adding (displaying)
graphical objects; erasing graphical objects; changing them; dispatching
events over their displayed graphical objects; etc.

Devices communicate changed area's to their super-device and eventually
to the window, which takes care of planning and executing the actual
redisplay of the X-window.

The PCE-user normally uses the behaviour of devices via the classes
figure or one of the sub-classes of window.  Class device itself may be
used for the definition of compound graphical objects.

The <-area of a device is by definition the bounding box of all
<-displayed graphical objects.

@see graphical<-common_device
@see graphical-device
@see topic Compound Graphicals
@see class graphical
@see class event
@see object->recogniser


## Instance variables {#class-device-instvars}

- device-bad_bounding_box: bool
    Indicates that the contents has been changed such that the bounding-box
    (area) needs to be recomputed.

- device-bad_format: bool
    Indicates the contents has been changed such that the attached format
    object needs to be recomputed.

    @see class format

- device-clip_area: area*
    If not @nil, no pixels outside this area are affected.  The area of the
    entire device is the intersection of the union of the area's of all
    graphicals and the clip_area.

    The variable is defined at the level of class device for technical
    reasons.  The clip_area mechanism is activated at the level of
    class figure.

    **Defaults**: @nil (no clipping is done).

- device<-format: format*
    When not @nil, the positions of the graphicals displayed on the device
    is computed using the attached format.

    If a device has a <-format, graphicals cannot be moved any more.  Also,
    the position argument of ->display is simply ignored.

    @see class format

- device<-graphicals: chain
    Chain of graphicals displayed on the device.  The first graphical is the
    one that is entirely in the background; the last is entirely in the
    foreground.  See `graphical ->expose` and friends to change the
    stacking order of overlapping graphicals.

    The contents of this chain may be queried but not changed.  Use the
    manipulation behaviour defined on devices and graphicals to change the
    contents of the device.  Note that the `collection enumeration` methods
    ->for_all and `->for_some` are also available for devices.  The
    following example changes the pen thickness of all graphicals of
    a device to 2:

    	send(Device, for_some, @default,
    		 message(@arg1, pen, 2)),

    @see device<-contains
    @see graphical-device
    @see device<-member
    @see device->for_some
    @see device->for_all

- device<-level: int
    Nesting depth from the window.  The window itself has <-level equals 0.
    Used for quick detection of a common device for two graphicals.

    See also ->reparent.

- device-offset: point
    Offset of the coordinate-system to the coordinate-system of it's device.
    Managed via the geometry-changing behaviour of devices (->position,
    etc.).

    @see device->position

- device<-pointed: chain
    Chain of (graphical) members whose bounding-box overlap the position of
    the mouse.  Internally used to generate area_enter and area_exit events
    and perform event-dispatching.  May be used from code that handles an
    event to detect which graphicals overlap the mouse-position.

    The first element of this chain is the top-most graphical.  The others
    are further and further in the background.

    @see device->update_pointed
    @see device<-pointed_objects

- device-recompute: chain
    @see graphical->compute


## Send methods {#class-device-send}

- device->advance: from=[graphical]*, propagate=[bool], direction=[{forwards,backwards}]
    Advance the keyboard-focus to the next graphical object of this device
    for which the test `Graphical ->_wants_keyboard_focus` succeeds.  Used
    by text_item to implement `text_item ->advance`.

    This method cycles through the <-graphicals chain.  If the
    argument is a graphical and there is no graphical that requests
    the keyboard focus further down the <-graphicals chain and the
    device is not the window itself, and `propagate` not is @off, it
    will invoke ->advance on its <-device, passing itself as
    argument.  This behaviour allows for handling the keyboard focus
    in dialog-windows built from nested devices.

    Otherwise, If no graphical wants to have the keyboard focus, the
    keyboard focus of the <-window is set to @nil.

    **Defaults**:
    If graphical is @default, the first graphical accepting
    `->_wants_keyboard_focus` is given the focus.

    @see text_item->_wants_keyboard_focus
    @see text_item-advance
    @see window->keyboard_focus

- device->append_dialog_item: item=graphical, relative_to_last=[{below,right,next_row}]
    Append a graphical object using the dialog-layout mechanism.
    This method may be used to define sub-dialogs.  See `dialog
    ->append' for details.

- device->changed_union: ox=int, oy=int, ow=int, oh=int
    If the union of the displayed graphicals (see `area->union`) has
    changed, this method is called.  The arguments indicate the old
    union of the graphicals.  This method may be used to implement
    custom scroll_bar objects with figure objects.

- device->clear: [{destroy,free,erase}]
    Remove all graphicals from the device.  The argument controls
    what happens to the removed graphicals.  By default it uses
    `device->erase`, leaving destruction to the
    reference-count garbage collector.  Using free the graphicals
    will be send a ->free, ensuring destruction of the graphicals,
    but not their parts and using destroy ensures deep destruction
    of the displayed graphicals based on `visual->destroy`.

    @see device->erase

- device->compute
    Send ->compute to all members of -recompute, next recomputes the
    <-format if requested by -bad_format and finally recomputes the <-area
    (bounding box) requested by -bad_bounding_box.

- device->display: graphical, position=[point]
    Display a graphical object on the device.  This is the principal way of
    drawing on a device.   It performs the following operations:

    - if `point` is not @default, invoke `graphical ->set`
    	   with the X and Y values of the point.

    - invoke `graphical ->device: device`

    - invoke `graphical ->displayed: @on`

    Note that the geometry management may be redefined for each
    graphical using `graphical ->geometry`.  Also `graphical
    ->device' and `graphical ->displayed` may have been redefined.

    **Defaults**:
    If point is omitted, the current position of the graphical is used.  For
    graphicals that were never displayed, this is usually (0,0)

    @see graphical->reparent
    @see graphical->display_on
    @see graphical-displayed
    @see graphical-device

- device->erase: graphical
    Erase a graphical from the device.  The graphical is removed from the
    <-graphicals chain of the device, the graphical's <-device is set to
    @nil and the display is updated.

    Note that the graphical is only destroyed if it had no other references.
    To make sure the graphical is destroyed, use `Graphical ->free`.
    To be sure all sub-graphicals are removed as well, use
    `Graphical->destroy`.

    @see graphical->reparent
    @see device->clear

- device->event: event
    Process an event.  It performs the following operations:

    - Update `Device <-pointed` and generate `area_enter` and
    	  `area_exit` events to graphicals that were added/removed
    	  from this chain.  Implemented by `Device ->update_pointed`

    - Post the event to the elements on `Device <-pointed` using
    	  `Event ->post`.  If `Event ->post` succeeds on some graphical,
    	  stop with success.

    - Activate `Graphical ->event` to process recognisers defined
    	  on the device.

    @see device->update_pointed
    @see graphical->event
    @see event->post

- device->for_all: [name], code
    Run code on all graphicals with given <->name.  Stops with failure
    executing code fails for some graphical.  If name is @default, code is
    executed on all member graphicals, which makes the method
    equivalent to

    	send(Dev?graphicals, for_all, Code)

    @see device->for_some
    @see chain->for_all
    @see device-graphicals

- device->for_some: [name], code
    Run code on all member graphicals that have <-name equal to name.  The
    exit status is ignored and ->for_some always succeeds.  If
    `name` is @default, code is run on all <-graphicals in the
    device object.

    **Defaults**: If name is @default, code is executed on all member graphicals.

    @see device->for_all
    @see device-graphicals

- device->foreground: colour
    The foreground colour of a device determines the colour used to draw
    member graphicals that have <-colour not equal to @default.  `Device
    ->foreground' is equivalent to `Graphical ->colour`.

    @see graphical->colour
    @see class colour

- device->format: format*|name, [any]
    This method has two modes of operation:

    - Device ->format: format*
    	Attaches a format object to a device.  This implies the
    	graphicals are placed in a two-dimensional grid.

    - Device ->format: name, value
    	Change an attribute of the associated format object and
    	recompute the layout of the graphicals in the device
    	according to the new format.

    See class format for details.

    @see class format

- device->geometry: x=[int], y=[int], width=[int], height=[int]
    `Device ->geometry` updates `Device <-offset` (the offset of the
    device's coordinate system relative to its super-device) such that
    the top-left corner is at (X, Y).

    The <-width and <-height of a device is defined to be the union of the
    area of the member graphicals (intersected with `Device <-clip_area`)
    and the W and H parameters of the geometry request are thus ignored.

    @see graphical->geometry

- device->initialise
    Create an empty device.

- device->layout_dialog: gap=[size], size=[size], border=[size]
    This method implements the layout of dialog objects (see
    `dialog ->layout`).  It may be used to define compound dialog
    windows or to exploit the layout mechanism for dialog windows in
    a normal window or graphical device.

- device->modified_item: graphical, bool
    Virtual method, fails.  Dialog item's send this message to
    their device after they have been modified.  If this message
    succeeds, they assume the dialog is an `attribute editor` and
    will not forward their <-message.  Otherwise they will
    immediately forward their <-message.  See also
    `dialog ->modified_item`.

- device->move: point
    Move the origin of the device to be at point.  Note that this need not
    be the top-left corner.  To set the top-left corner, use `Device ->set:
    X, Y'.

    @see device->y
    @see device->x
    @see device->position

- device->position: point
    *Inherits description from*: device-offset

    @see device-offset
    @see device->move

- device->reference: [point]
    Move the reference-point (offset) of the device to be at point, but move
    the member graphicals the same amount in the other direction, such that
    they remain displayed at the same position.

    The default reference point is the top-left corner of the bounding box
    of all the member graphicals.

- device->reparent
    Called from PCE graphics kernel if something has changed to the <-device
    organisation up in the <-device hierarchy.  This general method performs
    the following:

    - Updates `device <-level` to reflect the distance to the
    	`top-most` graphical device.

    - Sends ->reparent to all its member graphicals

    - Invoked `graphical ->reparent` on itself

    See `graphical ->reparent` for details.

    @see graphical->reparent

- device->resize: x_factor=real, y_factor=[real], origin=[point]
    Resize the member graphicals of the device in X and Y direction.  See
    `Graphical ->resize` for details on the parameters.

    **Defaults**:


    The following defaults apply:

    - Y-resize factor (2nd argument) defaults to the X-resize-factor
    	(1st argument).

    - The resize origin defaults to <-position (origin) of the
    	   device

    **Bugs**:

    The contents are resize by sending `graphical ->resize` to each
    graphical.  If  graphicals are related using constraints the result
    may be incorrect.

    @see graphical->resize

- device->room: area
    Test if there are no graphicals that have `Graphical <->displayed:
    @on' and overlap with area on the device.  See also <-inside.

    @see device<-inside

- device->selection: graphical|chain*
    Set the member graphicals that have `graphical <->selected: @on`.
    If the argument is a graphical, this graphical will be selected.  If it
    is a chain of graphicals, each member of this chain will be selected.
    If the argument is @nil, no graphical will be selected.

    All graphicals displayed on this device, except for the indicated one's
    will be deselected.

    @see device<-selection
    @see graphical-selected

- device->unlink
    ->erases all <-graphicals from the device and then calls
    `graphical ->unlink`.  Note that erased graphicals are only
    garbage-collected if they have no references from other objects.
    See also `visual ->destroy.

- device->update_pointed: event
    Update the `Device <-pointed` chain to represent all graphicals
    displayed on the device for which `event` is in the event_area.
    Graphicals that are deleted from the chain are send an `area_exit`
    event.  Graphicals that are added to the chain are send an `area_enter`
    event.   If on or more of the mouse-buttons are down, these events are
    `area_cancel` and `area_resume`.

    @see device->event
    @see device-pointed
    @see topic Event types

- device->x: int
    Set the X-coordinate of the origin. See `Device ->move`

    @see device->move

- device->y: int
    Set the Y-coordinate of the origin. See `Device ->move`

    @see device->move


## Get methods {#class-device-get}

- device<-catch_all: name -> graphical
    Find a displayed graphical by it's name.  To find a graphical named
    `hello`, type

    	?- get(Device, hello_member, Hello).

    NOTE:	Graphicals may also be found using <-member.

    **Diagnostics**:
    Asking for a graphical that does not exist will trap the no_behaviour
    error.

    @see device<-member

- device<-contains: -> chain
    Implementation of the visual part-of hierarchy.  Equivalent to `Device
    <-graphicals'.

    @see device-graphicals
    @see visual<-contains

- device<-find: at=[point|event], condition=[code] -> graphical
    Find the most `local` graphical overlapping with `at` for which
    `condition` succeeds.

    This methods uses a similar search technique as used by
    ->event and is first of all intended to help writing gestures
    that have to relate two graphical objects, possibly not placed
    on the same device object.  Examples are class connect_gesture
    or a `drag-and-drop` gesture.

    <-find performs the following steps:

    - Traverse the list <-graphicals from tail to head (i.e.
    	starting at the topmost graphical).  For each graphical:

    - if `at` is given and `graphical ->in_event_area` fails
    		Continue with the next graphical

    - if the graphical is a device object
    		Recursively call <-find on the device.  Return
    		with the result if any.

    - if the graphical is not a device
    		Evaluate the condition and return the graphical
    		if the condition succeeds.

    - Finally, evaluate the condition on the device itself and
    	return the device if the condition succeeds.

    The following example finds a graphical that defines the send-method
    `drop` in the same window as where the event occurred:

    	...
    	get(@event?window, find, @event,
    		message(@arg1, has_send_method, drop), Gr),
    	...

- device<-inside: area -> chain
    Returns a new chain object with all graphicals that are entirely inside
    the argument area.  If no graphicals can be found, an empty chain is
    returned.  Described with `selection` because it is commonly used to
    implement selecting multiple graphicals by dragging a box around
    them.   See PceDraw.

    @see device->room
    @see area->inside

- device<-member: graphical_name=name -> graphical
    Return the first graphical of `Device <-graphicals` that has name
    `name`.  This mechanism is commonly used to address parts of a
    device.

    **Diagnostics**: Fails if no graphical with the requested name exists.

    @see device<-catch_all
    @see device<-get_catch_all
    @see graphical-name
    @see device-graphicals

- device<-offset: -> point
    *Inherits description from*: device-offset

    @see device<-y
    @see device<-x
    @see device<-position

- device<-pointed_objects: at=point|event, append_to=[chain] -> chain
    Determines all objects that overlap with some position.  This method is
    used internally to determine the objects to which an event should be
    send.  The first argument describes the position.  It is either a point
    or an event.  In the latter case the position of the event is taken.

    If chain is supplied, this chain is filled with the matching graphicals.
    This chain is `chain->clear`ed first.  If it is @default, a new
    chain object with the matching graphicals it returned.

    The matching graphicals are the graphicals for which `Graphical
    ->in_event_area' succeeds.  The graphicals are stored in the chain, such
    that the top-most graphical is the head of the chain.

    @see device-pointed
    @see graphical->in_event_area

- device<-position: -> point
    *Inherits description from*: device-offset

    @see device<-offset

- device<-selection: -> chain
    Returns a new chain with all graphicals that have `Graphical <-selected:
    @on'.  If there are no such graphicals, an empty chain is returned.

    @see device->selection
    @see graphical-selected

- device<-x: -> int
    Returns the X-coordinate of `Device <-offset`.

    @see device<-offset

- device<-y: -> int
    Returns the Y-coordinate of `Device <-offset`.

    @see device<-offset

