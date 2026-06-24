# class graphical {#class-graphical}

PCE contains a large number of classes related to creating and
manipulating graphical objects.  The graphics facilities were designed
for interactively manipulating diagrams and graphical representations of
information and knowledge.  The graphics facilities are not suitable for
2-D or 3-D modeling of real-life objects (such as cars and planes).

The most commonly used subclasses of class graphical are:

	class box			Rectangle (filled, rounded)
	class ellipse
	class line
	class path			Multiple segment line (curved)
	class bitmap		Image
	class text			Short text objects
	class device		Compound graphical objects
	class dialog_item	Specialised items for dialogue

Example types of diagrams for which PCE has been used are the
representation of graphs (hierarchies, lattices, flow-charts), tabular
information, iconic representations of computer networks, knowledge
bases and various graphical editors.

A graphical describes an image, all of whose affected pixels are within
a rectangular area, represented by <-area.  The main role of class
graphical to facilitate the communication to its <-device.  A
device object is a collection of graphical objects.  Class
device is a subclass of class graphical.  This allows us to
display devices on devices, leading to nested graphicals with
local coordinate systems.

Graphics in PCE is more complicated than most of the other facilities,
although the maxim ``if your needs are simple, it will be simple in PCE
as well'' still applies.  Advanced use of graphics, however, requires a
good understanding of the basic concepts and how they are applied.

Mouse and keyboard events are made available to a graphical using the
method ->event.  By default the primitive graphical classes do not
respond to this message.  There are two ways to make a graphical
sensitive to the mouse and/or keyboard.  The first is to associate a
recogniser object using `object ->recogniser`.  The second is to
redefine the ->event method.

## Refining graphicals {#class-graphical-refining-graphicals}

A couple of methods are commonly redefined on class graphical
for creating customised graphical objects:

- ->event: event
	May be redefined to modify event processing.  This is a
	class-level alternative for `graphical ->recogniser`.

- ->device: device
	Called if the device of the graphical object is changed.
	May be redefined to take care of side-effects, but the
	user *must* call the super-behaviour.

- ->displayed: bool
	Called if the <-displayed attribute is changed.  Like
	->device, this method may be redefined, but the user
- must* call the super-behaviour.

- ->reparent: int
	Indicates that the consists-of tree of graphicals has
	changed between this graphical and the window.  May
	be redefined, but the user *must* call the super-behaviour.

@see class gesture
@see class handle
@see class dialog_item
@see class window
@see class device
@see object->recogniser
@see class area


## Class variables {#class-graphical-classvars}

- graphical.event_tolerance: 0.. = 5
    Minimum size of event-area.  This class-variable is used by
    `graphical ->in_event_area` and `line ->in_event_area`.

- graphical.selection_handles: {corners,sides,corners_and_sides,line}* = corners_and_sides
    Determines how ->paint_selected draws the selection indication,
    provided <-selected is @on and the <-window's
    `window <-selection_feedback` equals `handles`.   Values:

    - sides
    	Paint inverted blobs at the middle of all 4 sides of the
    	bounding box.

    - corners
    	Paint inverted blobs at the 4 corners of the bounding
    	box.

    - corners_and_sides
    	Combines the above possibilities

    - line
    	Specific for class line.

    - @nil
    	Don't draw any inverted blobs.  Used with classes that
    	define a private version of these functions
    	(`path ->paint_selected`).

- graphical.visual_bell: bool = @on
    @see graphical->flash
    @see window->flash
    @see graphical->alert

- graphical.visual_bell_duration: int = 100
    Length of flash in milliseconds

    @see graphical->flash
    @see window->flash


## Instance variables {#class-graphical-instvars}

- graphical<-active: bool
    If @off, the colour of the drawing pen will be set to
    <-class_variable_value: inactive_colour.  The method ->event
    will simply fail if <-active is @off.

- graphical-area: area
    The area object associated with each graphical describes a
    bounding-box inside which all pixels affected by the graphical are.  The
    area is used for:

    - Representing the position and size of the graphical.
    - Planning redraw of the window.
    - Dispatching events to graphicals `below` the mouse.

    The get method first invokes ->compute to update the graphical.

    **Defaults**: The default area of most graphicals is area(0,0,0,0).

- graphical<-colour: [colour|pixmap]
    The <->colour attribute of a graphical describes the colour used to draw
    the affected pixels.  It is either a colour object or @default.  The
    latter implies the <->colour attribute of the device is used.

    **Defaults**: @default.  For windows, the default colour is `@display <-foreground`.

    @see class colour

- graphical-connections: chain*
    Chain of connection objects.  Connections are lines relating two
    graphical objects.  Connection are automatically updated when either of
    the connected graphicals is updated.

    @see topic Connections

- graphical<-cursor: cursor*
    Cursor displayed in this window when the graphical is in focus of
    events.  When @nil, the cursor of the device is displayed.

    **Defaults**: @nil.

    @see class cursor

- graphical<-device: device*
    The device (normally picture, dialog or figure) on which the graphical
    is displayed.  A graphical can only be displayed on one device.  The
    <->device slot is used by graphicals to inform their device of changes,
    so the device can perform the appropriate redraw operations.  The
    following is always true:

    	If the device of a graphical is not @nil, the graphical is
    	member of `Device <-graphicals` of this device and
    	visa-versa.

    Normally, a graphical is assigned a device using `Device ->display`.

    @see graphical-displayed
    @see device->display
    @see device-graphicals
    @see class device

- graphical<-displayed: bool
    If @off, the graphical will not be displayed even if it is member of a
    device.  The initial value is @off.  It is set to @on by `device ->display`
    an @off by `device ->erase`.

    `Graphical ->displayed` is used to remove a graphical temporary
    from the display.  See also `figure ->status`.

    **Defaults**: Initially @off.  Set to @on by `Device ->display`.

    @see graphical<-is_displayed
    @see device->display
    @see dialog_item->show
    @see graphical-device

- graphical-handles: chain*
    Chain of handles.  Each handle defines a position to which a connection
    can attach.   See _T Connections_.

    Handles can be attached at the instance level and at the class level
    (see `Class ->handles`) or both.

    **Defaults**: @nil (no handles at the instance level)

    @see topic Connections
    @see graphical->handle
    @see class-handles
    @see class handle

- graphical<-inverted: bool
    If @on, `Graphical <->area` is inverted after the graphical is painted,
    inverting all pixels in the bounding-box of the graphical.  Note that
    this implies at inverted circle appears as a white circle in a black
    square (using white background and black foreground colours).  Also
    note that graphicals below (i.e.  hidden by this graphical) are inverted
    too.

    **Defaults**: @off

    @see graphical-selected

- graphical<->name: name
    The name of a graphical can be used to locate it on its device.  Finding
    graphicals by name is a simple but very effective way.  Graphicals
    generally do not visualise their <-name.

    See `device<-member` and `device <-catch_all`.

    @see frame<-get_catch_all
    @see device<-get_catch_all
    @see device<-member

- graphical<-pen: 0..
    The pen attribute denotes the thickness in pixels of the drawing pen for
    drawing lines.  It is defined at class graphical, but only applicable to
    classes whose images actually contain lines.  If the pen is 0 (zero),
    lines are not displayed.

    **Defaults**: 1

    @see graphical-texture

- graphical<-request_compute: any*
    When not @nil, indicates that the graphical is in inconsistent state.
    Used for graphical objects that require significant computation to
    determine their area and visual representation.

    When no argument is specified with ->request_compute the variable will
    be filled with @on.

    If a ->request_compute is invoked on a graphical, PCE's graphical kernel
    will invoke a ->compute on it if the graphical needs to be repainted or
    one of the dimensions of the graphical is requested.  See <-area,
    `display ->dispatch` and `->compute`.

    @see graphical->compute
    @see graphical->request_compute

- graphical<-selected: bool
    Indicates whether or not the graphical is selected.  The visual feedback
    is determined by the graphical.selection_style.

    @see graphical.selection_style
    @see graphical->selected
    @see graphical->toggle_selected
    @see device<-selection
    @see device->selection
    @see graphical-inverted

- graphical<-texture: texture_name
    Stipple pattern used for drawing lines.  Applicable to all graphicals
    that have line elements in them.  Values:

    	| none       | Straight line                                         |
    	| dotted     | Pattern: 1 on 2 off ...                               |
    	| dashed     | Pattern: 7 on 7 off ...                               |
    	| dashdot    | Pattern: 7 on 3 off 1 on 7 off ...                    |
    	| dashdotted | Pattern: 11 on 3 off 1 on 3 off 1 on 3 off 1 on 3 off |
    	| longdash   | Pattern: 15 on 7 off ...                              |

    @see graphical-pen


## Send methods {#class-graphical-send}

- graphical->_redraw_area: area
    Repaint the graphical.  Area specified the area (in the
    coordinate system of the current device) that needs to be
    repainted.  Area is guaranteed to overlap with <-area, the
    bounding box of the pixels affected by this graphical.

    This method is called by the graphical infra-structure of XPCE
    and should never be called directly.   See also ->draw and
    ->compute.

    This method may be redefined to create custom graphical objects.
    It is not obligatory for the redefinition to call the method of
    the super-class.   The definition should not apply paint outside
    the <-area of the graphical.  If there is no simple way to avoid
    this, use ->clip and ->unclip to ensure this.

    The definition can use ->draw to paint other graphical objects
    or one or more of the primitive drawing operations listed below:

    	| ->draw_arc   | Draw ellipse-part or pie-part |
    	| ->draw_box   | Draw rectangle                |
    	| ->draw_fill  | Fill/clear a rectangle        |
    	| ->draw_image | Draw an image object          |
    	| ->draw_line  | Draw a line segment           |
    	| ->draw_poly  | Draw a polygon                |
    	| ->draw_text  | Draw a string                 |

    The pen, colours, etc. may be manipulated using the
    following methods:

    	| ->save_graphics_state    | Save current settings |
    	| ->restore_graphics_state | Restore old values    |
    	| ->graphics_state         | Modify graphics state |

    The User Guide (Programming in PCE/Prolog) provides
    further documentation on redefining graphicals.

    Another common approach to create custom graphics is
    by subclassing class device to define compound
    graphicals.

- graphical->_wants_keyboard_focus
    Test if graphicals wants keyboard events (fail).  Used by `device
    ->advance' and redefined by various sub-classes.

- graphical->below: graphical*
    Put me below argument.

- graphical->left: graphical*
    Put me left of argument.

- graphical->right: graphical*
    Put me right of argument.

- graphical->above: graphical*
    Integrates plain graphical objects in the layout system
    defined for class dialog.  The ->above relation is stored using
    ->attribute.  This method handles combinations of plain
    graphical objects and dialog_item objects correctly.

    See also `dialog_item ->above`, `dialog ->layout` and
    `device ->layout_dialog`.

- graphical->alert
    Alert the user of an error.  Depending on the graphical.visual_bell, either
     ->flash or ->bell are invoked.

    **Defaults**: Depends on the resource Graphical.visual_bell.

    @see visual->report
    @see graphical->flash
    @see window->flash
    @see graphical->bell
    @see graphical.visual_bell
    @see display->inform
    @see editor-error_message

- graphical->reference: point
    Dialog item integration.

- graphical->alignment: {left,center,right,column}
    Dialog item integration.  These methods associate the requested
    value using an ->attribute. See `dialog_item ->alignment` and
    `dialog_item ->reference` for details.

- graphical->apply: [bool]
    Virtual method, defined by most subclasses of class dialog_item.
    This method should forward the <-message of the dialog_item if
    the dialog_item has been modified by the user or the argument
    is @on.  See also `dialog ->apply`

- graphical->area: area
    Changes position and size.  Implemented using `Graphical ->set`.

    @see graphical->set

- graphical->auto_align: bool
    *Inherits description from*: graphical<-auto_align

- graphical->bell: [int]
    Rings the bell on the associated display.  The argument is the volume.

    @see window->flash
    @see display->bell
    @see graphical->alert

- graphical->center: point
    Moves the graphical such that the center of its area becomes point.
    Moving is realised using `Graphical ->set`.

    @see graphical->set

- graphical->unclip
    Undo previous ->clip.

- graphical->clip: [area]
    Clip subsequent drawing actions to the indicated rectangle.  If
    the rectangle is @default, all operations are clipped to the
    <-area of the graphical.  Note that on entry of ->_redraw_area
    clipping is set to the argument area, which will normally be
    larger than <-area.

    Each ->clip must be undone by an ->unclip before ->_redraw_area
    returns.  See also ->_redraw_area, ->graphics_state
    ->draw_line, etc.

- graphical->colour: [colour|pixmap]
    @see graphical<-display_colour
    @see device->foreground
    @see class colour

- graphical->compute
    This method is invoked by PCE's graphical kernel when:

    - One of the area attributes is needed (X, Y, size, ...).
    - The graphical needs to be repainted.

    The ->request_compute ->compute mechanism is used by graphical objects
    that need complex computations to determine their area and appearance
    from some internal state.

    For example, a menu is determined by its member menu_items and several
    flags manipulating its visual appearance.  It would be a waste to
    recompute the entire layout of the menu on each change of its state as
    there will often be multiple changes before it actually needs to be
    repainted on the screen.  A menu will therefore just issue
    `graphical ->request_compute` on itself if recomputation is needed.  PCE
    graphics kernel will cause a message ->compute to be send to the
    graphical just before the ->_redraw_area.

    The definition of this method at the level of class graphical just
    assigns <-request_compute to @nil.

    @see device-recompute
    @see graphical-request_compute

- graphical->connect: to=[graphical], link=[link], to_kind=[name], from_kind=[name]
    Create a connection (line) to another graphical, which must be displayed
    on the same window, but not necessarily on the same device object.

    `graphical ->connect` simply invokes `link<-connection` passing
    the receiver as `from`, as well as `to`, `from_kind`
    and `to_kind`.  The normal way to redefine the type of
    connection established is using either `link->connection_class`
    or by redefining `link <-connection`.

    @see graphical->layout
    @see connection->initialise
    @see topic Connections
    @see graphical->handle
    @see graphical->disconnect
    @see graphical->connected
    @see class connection

- graphical->connected: to=[graphical], link=[link], to_kind=[name], from_kind=[name]
    Test if graphical has a connection that matches the specification.  Any
    of the values may be @default, which refers to `anything`.  See
    <-connections for a description of the arguments.  The method
    <-connected performs the same test, returning the connection
    object found.

    @see graphical<-connections
    @see topic Connections
    @see graphical->connect

- graphical->container_size_changed: width=[int], height=[int]
    <-width or <-height of <-contained_in changed.  Currently used
    only by class parbox.  See `parbox->line_width`

- graphical->corner: point
    Sets the corner opposite the origin of the graphical (see `graphical
    ->orientation') to the indicated position in the coordinate system of
    the graphical's device.

    This method used to support resizing graphicals: sending ->corner
    messages to with the location of the device resizes the graphical.
    Resizing is now dealt with by class resize_gesture.

    @see area<-corner
    @see graphical->set
    @see graphical<-corner
    @see graphical->corner_y
    @see graphical->corner_x
    @see graphical->orientation

- graphical->corner_x: int
    @see graphical->corner

- graphical->corner_y: int
    @see graphical->corner

- graphical->cursor: cursor*
    @see graphical->focus
    @see window-displayed_cursor

- graphical->device: device*
    Device I'm displayed on.

- graphical->disconnect: to=[graphical], link=[link], to_kind=[name], from_kind=[name]
    ->free all connections to- and from this graphical that match the
    arguments.  @default is taken to match anything.  The arguments are
    described with <-connections.

    @see topic Connections
    @see graphical->connect

- graphical->display_on: device*
    Display graphical on device.  Equivalent to `device ->display:
    graphical', which is the recommended way to display graphical objects.

    @see device->display

- graphical->displayed: bool
    If @on, graphical is visible.

- graphical->do_set: x=[int], y=[int], width=[int], height=[int]
    Set X, Y, W and H for graphical.   Unlike the other geometry management
    methods, this method calls ->geometry rather then ->request_geometry.

- graphical->draw: offset=[point], area=[area]
    Draw the graphical on the current device.  If `offset` is given,
    the coordinate system of the device is moved by the given offset
    prior to painting the graphical.  The optional argument is the
    changed area of the device.

    This method may be used to redefine ->_redraw_area.  It should
    not be called outside this context.

- graphical->draw_arc: x=int, y=int, w=int, h=int, angle1=[real], angle2=[real], fill=[colour|{foreground,background}]*
    Draw an ellipse-part.  X,Y,W,H define the bounding box if the
    entire ellipse.  Angle1 and Angle2 the start and end angles (in
    degrees).  When omitted, an entire ellipse will be drawn.

    Fill defines the fill colour.  If @nil, the shape is not filled.

    This method is part of the user-defined graphics infra-structure
    described with ->_redraw_area and should not be called outside
    this context.

- graphical->draw_box: x=int, y=int, w=int, h=int, radius=[0..], fill=[colour|{foreground,background}|elevation]*, up=[bool]
    Draw rectangular (rounded) box with the given X, Y, W, H.
    Radius defines the rounding radius for the corners.  If the last
    argument is a colour object, the interior will be filled with
    this colour.  If it is an elevation object, a 3D effect will be
    simulated.  In this case `up` defines whether the box is painted
    elevated or lowered.

    This method is part of the user-defined graphics infra-structure
    described with ->_redraw_area and should not be called outside
    this context.

- graphical->draw_fill: x=int, y=int, w=int, h=int, fill=[colour|{foreground,background}]*
    Fill rectangle with specified colour.  If the colour is @nil,
    the area is cleared to the current background.  If it is
    @default, the area is filled with the current foreground.

    To fill a rounded rectangle, use ->draw_box with the pen set
    to 0.

    This method is part of the user-defined graphics infra-structure
    described with ->_redraw_area and should not be called outside
    this context.

- graphical->draw_image: image, x=int, y=int, sx=[int], sy=[int], sw=[int], sh=[int], transparent=[bool]
    Draw a bitmap or pixmap image.  The image is drawn from X,Y.
    SX,SY,SW,SH define the source-area on the image.  The default
    area is the entire image object.  If the image is monochrome,
    (`image <-kind: bitmap`), the `on` pixels are painted in the
    current foreground and the off pixels in the current background.
    If transparent is @on, the off pixels are not painted.

    This method is part of the user-defined graphics infra-structure
    described with ->_redraw_area and should not be called outside
    this context.

- graphical->draw_line: x1=[int], y1=[int], x2=[int], y2=[int]
    Draw line segment from (X1,Y1) to (X2,Y2) using the current
    settings of the graphics state (see ->graphics_state).

    This method is part of the user-defined graphics infra-structure
    described with ->_redraw_area and should not be called outside
    this context.

- graphical->draw_poly: points=chain|vector, closed=[bool], fill=[colour|{foreground,background}]*
    Draw/fill a polygon.  Points is a chain object or vector object
    containing the points though which the polyline is drawn.  If
    closed is @on, a line is drawn from the last to the first point.
    If fill is specified, the interior is filled with the given
    colour.

    This method is part of the user-defined graphics infra-structure
    described with ->_redraw_area and should not be called outside
    this context.

- graphical->draw_text: string=char_array, font, x=int, y=int, w=[0..], h=[0..], hadjust=[{left,center,right}], vadjust=[{top,center,bottom}]
    Draw a string in the given font object.  If only X and Y are
    given, they describe the left-side of the baseline.  If W and H
    are given, the text is positioned in a box according the the
    alignment-specifiers hadjust an vadjust.  The box described by
    X,Y,W,H does not need to be large enough to contain the text.
    For example, a box of size 0.0 and hadjust/vadjust center/center
    may be used to center text around a point.

    Multiple lines are aligned according to hadjust (default: left).

    The redraw-method below draws a text-box:

    	'_redraw_area'(TB, _Area:area) :->
    		"Redraw text-box"::
    		get_object(TB, area, area(X,Y,W,H)),
    		send(TB, draw_box, X, Y, W, H),
    		get(TB, font, Font),
    		get(TB, string, String),
    		send(TB, draw_text, String, Font,
    		     X, Y, W, H, center, center).

    This method is part of the user-defined graphics infra-structure
    described with ->_redraw_area and should not be called outside
    this context.

- graphical->event: event
    Process a (user-) event.  This function is normally invoked by class
    device through `event ->post`.

    If the graphical has recogniser objects associated (see `object
    <-all_recognisers') and <-active equals @on, the recognisers are
    activated in the order they appear in this chain until one
    succeeds, after which this method returns successfully.

    This method is redefined by many subclasses of class graphical to
    achieve predefined response to user-events.  When programming
    user-defined (graphical) classes, this method is commonly used to
    attach predefined user-interface behaviour to the new class.

    The following creates a box that may be moved using the middle
    button:

    	?- new(B, box(100,100)),
    	   send(B, recogniser, new(move_gesture)).

    And the following example defines a class movable_box
    from which all instances can be moved:

    	:- pce_begin_class(movable_box, box).

    	:- pce_global(@movable_box_gesture,
    				  new(move_gesture)).

    	event(B, Ev:event) :->
    		(   send(B, send_super, event, Ev)
    		->  true
    		;	send(@movable_box_gesture, event, Ev)
    		).

    	:- pce_end_class.

    **Diagnostics**:
    Fails silently if there are no recognisers or none of the recognisers
    accepted the event.

    @see class recogniser
    @see object->recogniser
    @see object<-all_recognisers
    @see device->event

- graphical->expose: [graphical]
    Moves graphical in the stacking order of its device such that it is
    displayed just on top of the argument graphical.  If no argument
    is given, the grapical becomes the topmost one.

    See also ->hide, ->swap and ->overlap.

    **Defaults**:
    If the argument graphical is omited, the graphical will be moved to the
    top of the device's graphical stack.

    **Diagnostics**:
    Always succeeds but has no effect if the argument graphical is not
    displayed on the same device.

    @see graphical->swap
    @see graphical->hide

- graphical->flash: area=[area], time=[int]
    Alert the user of a possible problem by shortly inverting the graphical.
    Area specifies the area to be inverted, relative to the <-area
    of the graphical itself.  Time is the time in milliseconds which
    the graphical remains in the inverted time; the time of the
    total operation is this time, plus two time the required time
    to invert the area.  The default time the graphical is inverted
    is determined by graphical.visual_bell_duration.

    See also `window->flash`, `device->flash` and `tab->flash`.

    Normally one should invoke `graphical ->alert`, which depending
    on `graphical.visual_bell` will either ring the bell or flash
    the graphical.

    @see graphical->alert
    @see graphical.visual_bell_duration
    @see graphical.visual_bell

- graphical->focus: recogniser=[recogniser], cursor=[cursor], button=[name]
    When the graphical is related to a window, invoke

    	send(Window, focus, Graphical, Recogniser, Cursor, Button).

    Focusing further user-events to this graphical object.

    @see window->focus
    @see graphical->focus_cursor
    @see graphical->cursor

- graphical->focus_cursor: cursor*
    Invokes `window ->focus_cursor` it the graphical is displayed in a
    window.  Always succeeds.  This will modify the cursor of the window
    until the window's focus is changed.

    @see class cursor
    @see graphical->focus
    @see window->focus_cursor
    @see gesture-cursor

- graphical->generate_event: event_id
    Generate a new event-object and post it to this graphical.  The new
    event has the given id.  Its window is the window in which the graphical
    is displayed.  All other values default (to the values of the latest
    event).

    Used internally to generate area_enter and area_exit events from other
    pointer-oriented events.

    **Diagnostics**: Returns the value of `event ->post: graphical`

    @see event->initialise
    @see event->post

- graphical->geometry: x=[int], y=[int], width=[int], height=[int]
    Invoked from `graphical ->request_geometry`, which is, through
    `graphical ->set`, invoked by all the methods that manipulate
    the geometry of graphical objects.  The parameters are the
    requested X-, Y-, W and H values for the graphical.  Values that
    need not be changed are passed as @default.

    Sets the area and informs the graphical's device on the changed
    geometry.  The device will answer with a ->_redraw_area when this is
    necessary.

    This method is commonly refined, dealing with all possible resize and
    move requests.  If this method is refined if should *always* call the
    ->geometry method of the super-class.  It is allowed to change the
    parameters though  The example below defines a box fits on a 10x10
    pixel grid.

    	:- pce_begin_class(grid_box, box).

    	geometry(B, X:[int], Y:[int], W:[int], H:[int]) :->
    		align_to_grid(X, NX),
    		align_to_grid(Y, NY),
    		align_to_grid(W, NW),
    		align_to_grid(H, NH),
    		send(B, send_super, geometry, NX, NY, NW, NH).

    	align_to_grid(@default, @default).
    	align_to_grid(Value, Aligned) :-
    		Aligned is ((Value + 4) // 10) * 10.

    	:- pce_end_class.

    @see graphical->set
    @see device->geometry

- graphical->graphics_state: pen=[0..], texture=[texture_name], colour=[colour|pixmap], background=[colour|pixmap]
    Modify the graphics state.  Only the specified values will be
    modified.  This method will normally be called using the
    named-argument conventions of XPCE:

    	...,
    	send(Gr, graphics_state, pen := 2, colour := red),
    	...

    It is not allowed to return from ->_redraw_area with a modified
    state.  Therefore redraw-methods that modify the state should
    normally use ->save_graphics_state and ->restore_graphics_state.
    Here is a typical example, drawing a thicker line.

    	'_redraw_area'(MyGr, Area:area) :->
    		send(MyGr, save_graphics_state),
    		send(MyGr, graphics_state, pen := 2),
    		send(MyGr, draw_line, 0,0,100,100),
    		send(MyGr, restore_graphics_state).

    NOTE: It is generally advised to paint graphical objects that
    require the same graphics state together.  So, if you need to
    paint alternating thick and thin lines, first draw all thick
    lines and then all thin lines instead of switching the state
    between each line.

- graphical->handle: handle
    Associates a handle with the graphical.  A handle is a named
    connection-point, which position is determined by two expression objects
    in terms of the <-width and <-height of the graphical.  Connections
    between graphicals can only be made if both graphicals have suitable
    handles defined.

    Note that handles may also be attached at the class of the graphical.
    Such handles are used by all instances of the class.  See `class ->handle`.

    @see class handle
    @see graphical<-handle
    @see topic Connections
    @see graphical->connect
    @see class connection
    @see graphical-handles
    @see class->handle

- graphical->height: int
    Invokes `graphical ->set` width specified H-value.

    @see graphical->set

- graphical->hide: [graphical]
    Place in background or below argument.  If the argument is
    @default, the receiver becomes the deepest nested graphical or,
    in other words, the head of the `device<-graphicals` chain.

    See also ->expose, ->swap and ->overlap.

    @see graphical->swap
    @see graphical->expose

- graphical->in_event_area: x=int, y=int
    Used by class `device` to determine `device <-pointed_objects`.
    Performs the following steps:

    1. if the width or height is very small (< 5 pixels), it
    	is enlarged to make it at least 5 pixels.  Fails if
    	(X,Y) is outside this (enlarged) area.  The tolerance
    	is read from the graphical.event_tolerance.

    2. if the class has the C-function `class
    	-in_event_area_function' defined, this function is
    	called and its value is returned.  Otherwise success is
    	returned.

    **Bugs**:
    	# The 5 pixels should be a resource

    - The test should be a normal method, making it accesible to
    	the user.

    - Currently the C-function is only implmented for lines.  Must
    	be implemented for ellipses, circles, paths, etc.

    @see event->inside
    @see device<-pointed_objects
    @see class-in_event_area

- graphical->initialise: x=[int], y=[int], width=[int], height=[int]
    Initialise a graphical using the area components <-x, <-y, <-width and
    <-height.  It sets the following defaults:

    	<-displayed		@off  (will be set by ->display)
    	<-device			@nil
    	<-area			new area from the arguments
    	<-pen			1	1
    	<-texture			none
    	<-colour			@default
    	<-selected		@off
    	<-name 			Classname  (box, ellipse, ...)
    	<-handles			@nil
    	<-inverted		@off
    	<-cursor			@nil
    	<-request_compute	@nil

    Class graphical is a super-class of all graphical classes.
    Instances of this class can be created, but are probably of
    little use.

- graphical->keyboard_focus: [bool]
    If @off, release the `window->keyboard_focus`.  If @on or
    @default and the receiver passes the test ->_wants_keyboard_focus
    send	`window->keyboard_focus: Graphical`.

    See also `device ->advance`

- graphical->layout: attract=[real], nominal=[real], repel=[real], adapt=[int], iterations=[int], area=[area], network=[chain], move_only=[chain]
    When graphical is member of a `network` (see `graphical <-network`) of
    connected graphicals (see `graphical ->connect`), this method tries to
    give the entire graph of connected graphicals a sensible layout.  Don't
    expect too much: this algorithm does not know anything about what your
    graph is representing.

    The algorithm is based on [Eades:84]. It defines a number of springs
    between connected and non-connected nodes.  The algorithm does a
    Monte-Carlo simulation using these springs.

    The parameters are:

    - attract: real, default is 2
    	_Strength_ of the springs
    - nominal: real, default is 30
    	_Natural Length_ of the springs.  Individual connection objects may
    	have their specific `connection <-ideal_length`
    - repel: real, default is 2.0
    	Force (outwards) between any pair of non-connected graphicals
    - adapt: int, default is 15
    	Determines how much the network is changed each iteration
    - iterations: int, default is 100
    	Determines the number of iterations.
    - area: area
    	Area in which to constrain the graph.  By default the graph extends
    	unconstrained.   `Window <-visible` is often used to confine the graph
    	to the visible part of a window object.
    - network: chain
    	Network to layout.  By default these are the graphicals that can be
    	reached using <-network.  If provided explicitely only the provided
    	graphicals are affected.   Notably providing all nodes in a window
    	and using `window<-visible` for the area argument will layout each
    	network and spread the separated networks over the window.
    - move_only: chain
    	If present, only the given nodes are moved.  This is
    	useful for adding nodes to a given graph without
    	changing the layout of the already displayed nodes.

    Connections between objects are found using <-connected, which may
    be refined to deal with application specific situations.

    Visible annimation can be achieved calling this predicate in a loop using a
    low number of iterations each time and force a display update using
    ->flush between each iteration.

    **Defaults**: See description

    **Bugs**:
    # Gets slow if there are many nodes (> 50).
    - Produces bad results on large networks with many connections.

    @see graphical->connect
    @see class tree
    @see graphical<-network
    @see topic Connections

- graphical->move: point
    Synonym for `graphical ->position`: moves the origin of the graphical to
    the indicated position.  Normally the origin is the top-left corner.
    Note however that this depends on the `graphical <-orientation`.

    The origin of devices is the origin of the device's coordinate system.

    @see graphical->position

- graphical->normalise
    Equivalent to `graphical ->orientation: north_west`:  Ensures <-width
    and <-height are positive, making the origin the top-left corner of the
    graphical.

    @see graphical->orientation

- graphical->orientation: {north_west,south_west,north_east,south_east}
    Some graphical objects may have negative <-width and <-height.  The
    origin is the (X,Y) point of the graphical.  (X+W, Y+H) refers to the
    opposite corner (see <->corner).  This method places the origin at one
    of the indicated corner.  The graphicals location and size on the screen
    is not affected by this method.  The values are:

    	north_west	W>=0, H>=0	(->normalise)
    	north_east	W < 0, H>=0
    	south_west	W>=0, H < 0
    	south_east	W < 0, H < 0

    @see graphical<-size
    @see graphical<-orientation
    @see graphical->normalise
    @see graphical->corner

- graphical->paint_selected
    Paint the selection attributes if the graphical is <-selected
    and the <-window defines one of the following
    `window <-selection_feedback` values:

    - invert
    	Invert the bounding box.

    - handles
    	Request the graphical.selection_handles and paint the handles accordingly.

    - an elevation object
    	Elevate the bounding box.

    This method should not be called by the application programmer
    directly.  It is called by ->_redraw_area.

- graphical->pointer: point
    Move the pointer relative to the coordinate system of the graphical:
    computes offset relative to window's coordinate system and then invokes
    `window ->pointer`.

    **Diagnostics**: Succeeds without side-effects if the graphical is not displayed.

    @see window->pointer

- graphical->popup: popup
    Utility method to associate a popup with an object.  It performs the
    following steps.

    1. If the object has a `popup` instance variable, it will fill
    	this variable with the popup object and assume the class
    	knows how to display it.

    2. Otherwise it will attach an attribute `popup` to the object
    	and append the recogniser @_popup_gesture, which is a
    	default popup_gesture object

    **Bugs**: Notably 1) is a dubious assumption.

    @see class popup
    @see object->attribute
    @see class popup_gesture

- graphical->position: point
    Move graphical such that <-x and <-y match the x- and y-coordinates of
    the argument point.  Invokes `graphical ->set`.

    Synonym for `graphical ->move`.

    @see graphical->set
    @see graphical->move

- graphical->recogniser: recogniser
    Add recogniser for user events (last).  This is the normal way
    of associating event recogniser objects with graphical objects.

    Recognisers associated this way are store very similar to
    attributes associated with `object ->attribute`.  If the
    graphical is saved to a file using ->save_in_file, the
    recognisers will be saved with the graphical object.

    Using class-level programming, recognisers mat be associated
    with graphical objects in two ways: by redefining the
    ->initialise method, calling ->recogniser to associate event
    handling or by redefining the class `graphical ->event` method.

    The following example defines movable_box as a class:

    	:- pce_begin_class(movable_box, box).

    	:- pce_global(@movable_box_recogniser,
    			 	  new(move_gesture)).

    	event(MB, Ev:event) :->
    		"Associate @movable_box_recogniser"::
    		(   send(MB, send_super, event, Ev)
    		;   send(@movable_box_recogniser, event, Ev)
    		).

    	:- pce_end_class.

    See also: class gesture, class event, ->prepend_recongiser,
    `event ->post`, pce_global/2.

- graphical->redraw: [area]
    Request the graphical to repaint the specified area.  When @default, the
    entire graphical is repainted.  Should not be necessary for the
    application programmer as PCE takes care of redraw automatically.  See
    also ->request_compute.

- graphical->relative_move: point
    Move the graphical relative to its current position using the X- and
    Y-values of ^point^.

- graphical->reparent
    Updates the <-device of possible associated connections.  This method.
    It is invoked from class device when the position of the graphical in
    the consists-of tree relative to its window has changed:

    - When the graphical is first connected to a device.
    - When the graphical is erased from its device.
    - When the device received a ->reparent.

    @see device->erase
    @see device->display
    @see device->reparent

- graphical->request_compute: [any]*
    @see graphical-request_compute

- graphical->request_geometry: x=[int], y=[int], width=[int], height=[int]
    Invoked internally from `graphical ->set`, which in turn is invoked by
    all the methods that manipulate the geometry of graphical objects.  The
    parameters are the requested X-, Y-, W and H values for the graphical.
    Values that need not be changed are passed as @default.

    Invokes ->geometry to change the <-area.  The indirection ->request_geometry
    ==> ->geometry may be redefined when a graphical is constrained
    by some other object.  For example, class window exploits this
    mechanism to let windows communicate with their tile object.

- graphical->resize: factor_x=real, factor_y=[real], origin=[point]
    Realises together with `device ->resize` resizing of collections of
    objects.  This method resizes the graphical with specified factor
    relative to the given origin.  The X- and Y-distance of each of the
    corners to this reference point is multiplies by resp.  the X- and
    Y-resize factor (1st and 2nd argument).

    **Defaults**:
    The following defaults apply:

    - Y-resize factor (2nd argument) defaults to the X-resize-factor
    	(1st argument).

    - The resize origin defaults to <-x and <-y of the graphical.

    @see area->decrease
    @see text->resize
    @see graphical->set
    @see device->resize

- graphical->rotate: int
    Exchanges <-width and <-height if the rotation is 90 or 270 degrees.
    <-center is maintained.

    **Bugs**:
    Someday, xpce should support real rotation and scaling at the level of
    class device.

- graphical->restore_graphics_state
    Restore saved pen, texture, colours and font.

- graphical->save_graphics_state
    Save the current setting for the device's pen, texture and fore-
    and background.  The old values are restored using
    ->restore_graphics_state.  Each ->save_graphics_state must be
    closed with a ->restore_graphics_state before ->_redraw_area
    returns.  See also ->graphics_state.

- graphical->selected: bool
    @see graphical->toggle_selected
    @see graphical-selected

- graphical->set: x=[int], y=[int], width=[int], height=[int]
    Set the X-, Y-, W- and H- parameter of the graphical.  This method is
    invoked by all the other geometry managing methods (->size, ->position,
    ->x, ...).

    Note that this method is faster than ->size, ->position, etc.  when
    the geometry has to be changed from a set of integer values as no
    intermediate object has to be created.

    It first determines whether any action is necessary (i.e. there is a
    non-default value that is not equal to the current value).  If so, it
    invoked `graphical ->geometry` which takes care of the class-dependent
    geometry-changing action.

    **Bugs**:
    `graphical ->geometry` is invoked using the internal function
    `simpleSend()`, which bypasses possible object-level methods.  Geometry
    management can only be programmed at thee class level.

    @see graphical->height
    @see graphical->width
    @see graphical->resize
    @see graphical->size
    @see graphical->corner
    @see graphical->position
    @see graphical->y
    @see graphical->x
    @see graphical->geometry
    @see graphical->center
    @see graphical->area

- graphical->size: size
    Invokes `graphical ->set` with specified W- and H-parameter.

    @see graphical->set

- graphical->swap: graphical
    Swap the positions of both graphicals in the device's <-graphicals
    chain.  The `background` graphicals are at the head of this chain, wile
    the `foreground` graphicals are at the tail.

    @see graphical->expose
    @see graphical->hide

- graphical->texture: texture_name
    Stipple pattern of drawing pen.

- graphical->toggle_selected
    If <-selected equals @on, invoke ->selected: @off and visa-versa.
    Useful for implementing selection handling.

    @see graphical->selected
    @see graphical-selected

- graphical->unlink
    Delete possible connections using ->disconnect  and remove the graphical
    from it's <-device.

- graphical->width: int
    Invokes `graphical ->set` with specified X-value to set the width of the
    graphical.

    @see graphical->set

- graphical->x: int
    Invokes `graphical ->set` width specified X- value.

    @see graphical->set

- graphical->y: int
    Invokes `graphical ->set` width specified Y- value.

    @see graphical->set


## Get methods {#class-graphical-get}

- graphical<-absolute_position: [device] -> point
    Returns a point object indicating the position of the top-left corner of
    the graphical in the coordinate system of the argument device (or
    window).

    The position relative to the screen may be found using <-display_position.
    See also <-position and <-area.

    **Defaults**:
    When device is omited, returns the absolute position relative to the
    `top-most` device.  If the graphical is displayed, this is the window in
    which the graphical is displayed.

    **Diagnostics**:
    Fails silently if device is specified and graphical is not displayed on
    this device.

    @see graphical<-position
    @see graphical<-display_position
    @see graphical<-absolute_y
    @see graphical<-absolute_x

- graphical<-absolute_x: [device] -> int
    X coordinate of graphical in coordinate system of device.  See
    `graphical<-absolute_position`.

    @see graphical<-absolute_position

- graphical<-absolute_y: [device] -> int
    Y coordinate of graphical in coordinate system of device.  See
    `graphical<-absolute_position`.

    @see graphical<-absolute_position

- graphical<-alignment: -> name
    Dialog_item integration.  See ->alignment and `dialog_item ->alignment`
    for details.

- graphical<-all_recognisers: create=[bool] -> chain
    *Inherits description from*: object<-all_attributes

- graphical<-auto_label_align: -> bool

- graphical<-auto_value_align: -> bool

- graphical<-reference: -> point

- graphical<-auto_align: -> bool
    ->auto_align is for the integration with class dialog_item.  It
    associates an ->attribute auto_align with the specified value.
    <-auto_align returns the value of this attribute if present.
    Otherwise @on if one of the attributes <-above, <-below, <-left,
    <-right exists and @off otherwise.

- graphical<-bottom_side: -> int
    Y-coordinate of the bottom-side of the graphical in the coordinate
    system of its device.  With positive height, this is <-y + <-height.
    With negative height, this is <-y.

    @see graphical<-top_side
    @see graphical<-right_side
    @see graphical<-left_side

- graphical<-center: -> point
    @see graphical<-center_y
    @see graphical<-center_x

- graphical<-center_x: -> int
    @see graphical<-center

- graphical<-center_y: -> int
    @see graphical<-center

- graphical<-common_device: with=graphical -> device
    Graphical devices may be used to create complex diagrams by combining
    simple and other complex diagrams (see class device).  This method
    locates the most `local` device that displays both the receiver and the
    argument graphical.

    `Graphical <-common_device` is used internally to determine the device
    on which to draw connections between graphicals that are not displayed
    on the same device.

    **Diagnostics**:
    Fails (silently) if no such device can be found.  Note that this method
    always succeeds if both graphicals are displayed in the same window and
    always fails if they are displayed in different windows.

    @see class connection
    @see class device

- graphical<-connected: to=[graphical], link=[link], to_kind=[name], from_kind=[name] -> connection
    Find specified connection object.  See ->connected for details.
    If there are multiple connections matching the search the first
    one is returned.  See also <-connections.

    This method may be redefined to hook the behaviour of ->layout.

- graphical<-connections: to=[graphical], link=[link], from_kind=[name], to_kind=[name] -> chain
    Compute a new chain with all connections of graphical.  The arguments
    are:

    	| to:        | Graphical at other side             |
    	| link:      | Link that must be in the connection |
    	| from_kind: | Name of the handle at the from-side |
    	| to_kind:   | Name of the handle at the to-side.  |

    Fails if there are no connections.

    **Defaults**: Omitted or default arguments imply any value is legal.

    @see graphical<-network
    @see graphical->connected

- graphical<-contained_in: -> device|node
    Device this graphical is displayed on when not @nil.  If the
    device is a tree object, it will return the associated node
    using `graphical <-node`.

    **Diagnostics**: Fails if <-device equals @nil.

    @see tree<-contains
    @see graphical<-node

- graphical<-convert: object -> graphical
    General object are converted into graphicals by invoking <-image on the
    object.  This simplifies manipulation of non-graphical objects that
    manipulate a graphical (such as class `node`).

    @see node-image

- graphical<-corner: -> point
    New point defined as:

    	point(<-x + <-width, <-y + <-height)

    @see graphical->corner
    @see graphical<-corner_y

- graphical<-corner_x: -> int
    X-coordinate of the corner, defined as

    	<-x + <-width.

    @see graphical<-corner_y

- graphical<-corner_y: -> int
    Y-coordinate of the corner, defined as

    	<-y + <-height

    @see graphical<-corner
    @see graphical<-corner_x

- graphical<-display: -> display
    Display on which the graphical is displayed.  Useful when using multiple
    displays (see class display).

    **Diagnostics**:
    Fails (silently) if the graphical is not displayed (indirectly) on a
    window.

    @see graphical<-window

- graphical<-display_colour: -> colour|pixmap
    Return the colour in which the graphical is actually displayed.  If the
    variable `graphical <-colour` equals @default, this is the
    <-display_colour of the graphical's device.  Otherwise it is the value
    of the <-colour variable.

    If the graphical is displayed, this will always return a colour.
    Otherwise this method fails if neither the graphical, nor its device
    specify the colour.

    @see window->colour
    @see graphical->colour

- graphical<-display_position: -> point
    Determines the position of the top-left corner of the graphical relative
    to the display.  Sometimes use to determine the position of a new
    (prompter) window.  Note that the position of the current event,
    relative to the display may be found using

    	get(@event, position, @display, Point)

    See `event <-position` and @event.   See also <-position and
    <-absolute_position.

    **Diagnostics**: Fails if graphical is not related to a window

    **Bugs**:
    Yields incorrect results if the window is not actually displayed on the
    screen.

    @see graphical<-absolute_position

- graphical<-distance: graphical -> int
    Invokes `area <-distance: area` using the areas of both graphicals.

    **Bugs**: Silently assumes that both graphicals are displayed on the same device.

    @see graphical<-distance_y
    @see graphical<-distance_x
    @see area<-distance

- graphical<-distance_x: graphical -> int
    Yields an integer representing the distance between the graphicals in
    X-direction.  Yields 0 if both areas overlap in X-direction.

    @see area<-distance_x
    @see graphical<-distance_y
    @see graphical<-distance

- graphical<-distance_y: graphical -> int
    @see graphical<-distance_x
    @see graphical<-distance

- graphical<-frame: -> frame
    Frame on which the graphical is displayed.

    Frames are commonly used to represent a tool of an application (or an
    entire application).  This method provides an easy way to find the
    application object from some `deep-down` part of it.

    **Diagnostics**: Fails silently if the graphical is not displayed.

    @see graphical<-window
    @see frame<-frame

- graphical<-handle: name -> handle
    Returns handle object with specified name.  First scans `graphical
    -handles' followed by <-handles of the graphical's class.  The first
    matching handle is returned.

    **Diagnostics**: Fails silently when there is no matching handle.

    @see graphical<-handle_position
    @see graphical<-handles
    @see graphical->handle

- graphical<-handle_position: name=name, device=[device] -> point
    Determine position of the named handle in the coordinate system of the
    given device.  The default device is the graphicals <-device.

    **Defaults**:
    When no device is specified, the coordinate system of the graphical's
    device is used.

    **Diagnostics**:
    Fails if there is no such handle or the graphical is not displayed on
    the given device.

    @see handle<-y
    @see handle<-x
    @see graphical<-handle

- graphical<-handles: near=[point], kind=[name], distance=[int] -> chain
    New chain with all handles (both from -handles as from `class <-handles`
    that are within distance pixels from the given point and have the given
    kind. Fails if no handles match the criterion.

    @see graphical<-handle

- graphical<-height: -> int
    Height in pixels.  Note that for some graphicals this value may be
    negative, indication the top-side is above rather than below the
    y-position.

- graphical<-is_displayed: [device] -> bool
    Tests whether graphical is actually displayed, assuming `device` is
    displayed.

    **Defaults**:
    When device is @default, succeeds if graphical and all deviced above it
    have <-displayed: @on.

    @see graphical-displayed

- graphical<-left_side: -> int
    @see graphical<-bottom_side

- graphical<-network: link=[link], from_kind=[name], to_kind=[name] -> chain
    Computes a new chain, holding the receiving graphical and all graphicals
    that can be reached from it by following the specified types of
    connections:

    - link
    	When specified, follow only connections of this link

    - from, to
    	When specified, follow only links that are connected to the
    	named handles

    This method is save on cyclic graphs.  See also ->layout.

    @see graphical->layout
    @see graphical<-connections

- graphical<-node: -> node
    Return the node object that has `node <-image` equal to this
    graphical if the graphical is controlled by a node of a
    tree object.  See also <-contained_in.

    **Diagnostics**: Fails silently when the node cannot be found.

    **Bugs**:
    Only works if the node is attached to a tree and actually displayed. It
    checks whether the graphical's device is a tree and then uses `node
    <-find_node' to find the node object belonging to this graphical.

    @see graphical<-contained_in
    @see node<-find_node
    @see tree->event
    @see node-image
    @see class node

- graphical<-orientation: -> {north_west,south_west,north_east,south_east}
    *Inherits description from*: graphical->orientation

    @see graphical->orientation

- graphical<-position: -> point
    @see graphical<-absolute_position

- graphical<-right_side: -> int
    @see graphical<-bottom_side

- graphical<-size: -> size
    New size object created from <-height and <-width of the graphical's
    area.  Either or both of <-width and <-height may be negative.  See
    <->orientation.

    @see graphical->orientation

- graphical<-top_side: -> int
    @see graphical<-bottom_side

- graphical<-window: -> window
    Window on which the graphical is displayed.  Yields itself if graphical
    is a window.

    **Diagnostics**: Fails silently when graphical is not displayed on a window.

    @see graphical<-display
    @see graphical<-frame

