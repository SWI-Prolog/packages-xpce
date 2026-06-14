# class window {#class-window}

A window is a subwindow of a frame object.  It appears on the screen as a
rectangular area inside a frame.  It defines an infinite two-dimensional
drawing plane of which a rectangular sub-area is <-visible.  A window
optionally has ->scrollbars attached that allows the user to move the
visible rectangle over the infinite plane.

Class window takes care of the communication with its <-frame and the
window system it uses for it's graphical representation.

As defined in class device, a window can display graphical objects (i.e.
instances of one of the subclasses of class graphical).  There are several
kinds of windows:

- Class picture
  Normally used for representing drawings or diagrams.  Can
  contain all types of graphical objects.  By default has
  scrollbars.

- Class dialog
  Normally used for defining dialog boxes.  Normally contains
  instances of one of the subclasses of class dialog_item.  Does
  automatic layout.  By defaults has no scrollbars.

- Class browser
  Displays a single instance of class list_browser to examine a
  list of objects.

- Class view
  Displays a single editor to edit/browse a text.

@see class picture
@see topic Window Layout
@see class tile
@see class graphical
@see class frame
@see class event


## Class variables {#class-window-classvars}

- window.size: size = size(200,100)
    Default size of a window.  Normally applications have several windows
    and decide on the size and placement of each of these windows itself.


## Instance variables {#class-window-instvars}

- window<-background: [colour]
    Background colour of the window.

- window-bounding_box: area
    Union of graphicals in this window in the coordinate system of
    the window.

- window<-current_event: event*
    Event that is currently being processed by this window or @nil if this
    window is not processing any events.   This variable is managed by
    `window ->event`.

    This is not necessarily the same as @event.  Consider the case were the
    window has opened a popup.  In this case `window <-current_event` will
    point to the event that caused the popup to open (generally an
    ms_right_down event), while @event will point to the various events that
    are processed during the opened state of the popup window.

    @see window->event

- window<-decoration: window_decorator*
    Window displaying me and my decorations.  This slot will
    normally contain an instance of class window_decorator.  The window
    decoration displays a label above the Window and/or adds scrollbars
    to the window.  ->catch_all creates a decorator for methods that are
    not defined on class window and are defined by class window_decorator.
    This allows for e.g.,

        send(Window, scrollbars, both)

- window-displayed_cursor: cursor*
    Currently visible cursor.  This attribute is computed dynamically.  The
    actually displayed cursor of a window is:

    - If there is a focus and a <->focus_cursor: the <->focus_cursor
    - If there is a focus and the object in focus has a <->cursor
	   the cursor of the object in focus
    - If the mouse is a a graphical with a cursor: the cursor of the
	   most local and exposed graphical with a cursor defined.
    - In all other cases the <->cursor of the window.

    @see window->event
    @see window-focus_cursor
    @see graphical->cursor
    @see window->focus

- window<-focus: graphical*
    Graphical object in focus of events.  When not @nil, events will be sent
    to this graphical.  When <-focus_recogniser too is not @nil the event
    will be posted to this recogniser only.

    Note that keyboard events will be send to the <-keyboard_focus when
    this is not @nil.

    @see window->event
    @see window-keyboard_focus
    @see window->focus
    @see window-focus_recogniser

- window<-focus_button: [button_name]*
    When a focus is set (<-focus is not @nil), this focus remains active
    until a button-up-event is received with this button.  When @nil, the
    focus remains set till it is explicitly cleared.

    @see window->event
    @see window->focus

- window<-focus_cursor: cursor*
    When not @nil and <-focus is not @nil, this cursor will be displayed.

    @see window->focus_cursor
    @see window-displayed_cursor

- window<-focus_recogniser: recogniser*
    When not @nil and <-focus is not @nil, events are given to this
    recogniser object.

    @see class gesture
    @see window->event
    @see window-focus

- window-frame: frame*
    Frame object of which the window is a `frame<-member`.
    If the window has no frame and is not displayed on another
    graphical and create is @on or @default a default frame is
    attached to the window which is returned.

    @see frame<-frame
    @see window->above

- window<->has_pointer: bool
    If @on, pointer (mouse) is in window.  The method is invoked
    from ->event and may be redefined, but should always invoke
    the primary implementation.

- window<-input_focus: bool
    When @on, this window is the window receiving keyboard input.
    See also `frame ->input_focus`.  When modified, and the
    <-keyboard_focus is not @nil, the method will generate
    either an activate_keyboard_focus or a deactivate_keyboard_focus
    event to <-keyboard_focus.

    @see frame<-input_focus
    @see frame->input_focus

- window<-keyboard_focus: graphical*
    Graphical object in the window to which keyboard events should be
    posted.  When @nil, keyboard events are dispatched the same way as
    pointer events.

    A graphical getting the keyboard focus will be sent an
    `obtain_keyboard_focus` event.  Graphicals loosing the keyboard focus
    will be sent a `release_keybaord_focus` event.

    @see window->event
    @see window-focus
    @see window->keyboard_focus

- window<->popup: popup*
    Popup menu associated with this window.  It is activated through the
    global object @_popup_gesture when all other attempts to handle the
    button-event have failed.  See `window ->event`.

    @see window->event
    @see class popup

- window<-resize_message: code*
    The message is executed after the window has been resized by its frame.
    It allows the programmer to reorganise the contents of the frame such
    that they better fit the new size.  The arguments are:

	| @receiver | The window             |
	| @arg1     | The window             |
	| @arg2     | New size of the window |

    This message is always sent when the associated frame is created.

    Resizing a window may also be specified by redefining the
    ->geometry method.   See also <-visible and `dialog->layout`.

    @see frame->set

- window-scroll_offset: point
    Indicates how much the window is scrolled.  The top-left corner of the
    window is -in the coordinate system of the displayed graphicals-
    negative the x- and y- parameters of this point.

    `window <-visible` is the proper way to get information on the current
    location of the visible area of the window.

- window<-selection_feedback: {invert,handles,colour}|elevation|colour*
    The value of this variable determines how graphicals with
    <-selected: @on are visualised.  Values:

    - invert
      Swap the <-foreground and <-background colour.

    - handles
      Paint inverted blobs at positions defined by the
      graphical.selection_handles.  Used (for example) in
      PceDraw.

    - an instance of class elevation
      Paint the graphical and then elevate the bounding box
      according the the elevation object. Using a light-grey
      background is a good choice.

    - a colour object
      Ignore the `graphical <-colour` attribute, drawing the
      graphical using the colour specified here.  Suitable
      on colour devices only.  Using a light-grey or otherwise
      coloured `window <-background` generally improves the
      result.

    @see `graphical ->paint_selected`
    @see `graphical.selection_handles`.

- window<->sensitive: bool
    Determines whether or not the window is sensitive for events.  When
    @off, all events are ignored. Rarely used.

    @see window->event

- window-tile: tile*
    Tile object associated with this window.  The associated tile
    negotiates the layout of windows in the frame with the frame and
    finally dictates the size and position of the window.  If the
    window has no tile, a this method creates a tile object for the
    window and returns the new tile.

    @see window-display_area
    @see window->border


## Send methods {#class-window-send}

- window->_compute_desired_size
    This method is invoked by class frame to as part of `frame ->fit`.  It
    allows the window to resize itself to fit with its contents.  By default
    this method is only active for class dialog.

    This method may be redefined.

- window->above: window|tile
    Part of the definition of the layout of windows inside a frame.
    Requests the receiver to be placed above the argument window.

    Depending on whether or not the argument is a window or a tile,
    the behaviour of this method is rather different.  Using window
    argument is intended for initial creation of windows, while
    using a tile argument is intended for adding windows to an
    already open frame object:

    - window object argument
	If the window is part of a `vertical` stack, place the
	new window at the top of the stack.  If the window is
	part of a `horizontal` stack, create a vertical stack
	holding the receiver and stack of the argument window.
	If the window is not part of a stack, create a vertical
	stack holding both windows.

    - tile object argument
	Align the receiver immediately above the argument tile,
	giving it the same width.  If the argument is part of a
	vertical stack, insert the receiver at the proper
	location in the stack.  If the argument is part of a
	horizontal stack, create a vertical stack holding the
	argument and the receiver, and let this new tile object
	replace the argument tile in the tile hierarchy.

	To align a window nicely above another window, use:

		send(NewWindow, above, Old?tile).

	To align a window above two horizontally
	aligned windows, use:

		send(NewWindow, above, Old?tile?super).

    If the receiver window is not already part of the same frame as the
    argument window it will be made part of this frame.  Thus method may be
    used after one or both windows have been created.

    The `techniques` chapter in the Users's Guide contains a section
    on generation window layout in a frame.

    @see frame->append
    @see window-frame
    @see topic Window Layout
    @see window->right
    @see window->left
    @see window->below

- window->background: colour|pixmap
    Colour of the drawing plane.  Changing the colour forces a redraw.

    **Defaults**: The `display<-background`.

    @see window->foreground
    @see display-background

- window->below: window|tile
    Similar to ->above, but rhe receiver is placed below the argument.

    @see topic Window Layout
    @see window->above

- window->bubble_scroll_bar: scroll_bar
    Update bubble of given scroll_bar object.  Called by `scroll_bar
    ->compute'.

- window->catch_all: name, unchecked ...
    Handle delegation to <-decoration, <-frame and <-tile while
    these slots are not yet filled.  If the selector is found on any
    of these classes, the corresponding object is associated to
    the window and the message is delegated.  This implementation
    ensures that messages like `window_decorator ->scrollbars`,
    `frame ->kind` and `tile ->hor_stretch` may always be send
    directly to the window.

- window->changed_union: ox=int, oy=int, ow=int, oh=int
    This method is invoked from class `device` when the union of all
    graphicals has changed.  Used to update the scrollbars accordingly.
    See also ->update_scroll_bar_values.

- window->foreground: [colour]
- window->colour: [colour]
    Set the default colour for all graphicals displayed on this window: each
    graphical that has its colour set to @default will be displayed in the
    colour specified.  If the window is displayed, it will be redrawn.

    **Defaults**: When @default is provided, `display <-foreground` is used.

    @see window->foreground
    @see graphical<-display_colour
    @see window<-foreground
    @see class colour

- window->create: [window]
    `Window ->create` establishes the resources for making the window visible.
    This method is normally called from `frame->open` and should not be used
    directly by the user.

    @see frame->create
    @see window->open

- window->decorate: area=[{grow,shrink}], left_margin=[int], right_margin=[int], top_margin=[int], bottom_margin=[int], decorator=[window]
    Attach a window_decorator object to this window for displaying
    scroll_bar objects or a label.  This method is used by
    `window_decorator ->initialise`.

- window->destroy
    Destroys the associated window.  See `visual ->destroy` for details.

- window->expose
    Exposes (take to the front) the frame of the window on its
    screen.  Invokes `frame ->expose`.

- window->flash: area=[area], time=[int]
    Flashes the window by temporary inverting it.  Normally, applications
    should invoke `graphical ->alert`, which depending on the setting of the
    `window.visual_bell` will either ring the bell or flash the object.

    @see graphical.visual_bell_duration
    @see graphical.visual_bell
    @see graphical->bell
    @see graphical->alert

- window->flush
    Updates all graphicals in this window and requests a redraw for the
    window system.

    @see graphical->flush
    @see display->flush

- window->focus: graphical*, [recogniser]*, [cursor]*, [name]*
    Forward events arriving on this window to some graphical.  This method
    is normally used only by class gesture to process all events starting
    with a mouse-down and ending with the mouse-up of the same mouse-button.

    _Graphical_ is the graphical object in focus.  If _Recogniser_ is not
    @nil, events are given to this particular recogniser object. If _Cursor_
    is not @nil, the cursor is set to this value until the focus is
    released. The _Name_ argument indicates the button that initiated the
    focus. If a button-up event is received of this button, the focus will
    be released automatically. If the value is @nil, the focus remains
    active until a new `Window ->focus` message is received.

    See also ->grab_pointer and ->grab_keyboard.

    **Defaults**:
    The following defaults apply:

    | Recogniser  | @nil (None)								    |
    | Cursor      | @nil (Cursor is not changed)					    |
    | Button      | Button of the current event, computed with: Window?current_event?button |

    @see graphical->focus
    @see window-focus_button
    @see window-focus
    @see window-displayed_cursor
    @see window->grab_pointer
    @see class gesture
    @see window->keyboard_focus
    @see event->post
    @see window->focus_cursor

- window->focus_cursor: cursor*
    Changes the <-focus_cursor variable and displayed cursor if the window
    has <->focus not equal to @nil.  The cursor is automatically reset if
    the focus of the window is set to @nil.  This method is often used by
    gestures to change the cursor while the gesture is active.

    @see class cursor
    @see window-focus_cursor
    @see graphical->focus_cursor
    @see window->focus

- window->free
    `window ->free` causes the entire frame and possible other windows that
    are part of the frame to be freed.

    If a window is just to be deleted from its <-frame use `frame ->delete`.

- window->geometry: x=[int], y=[int], width=[int], height=[int]
    Traps the ->set, ->size, ->position and other methods to
    manipulate the geometry that are inherited from class graphical.  It
    sets the `<-area` of the window.

    Note that the size and position of a window is dictated by its
    associated <-tile.  To change the layout of windows in a frame, please
    examine the behaviour defined on class tile.  Windows delegate to their
    tile.

- window->grab_pointer: bool
    If @on is specified, all subsequent events will be reported as
    if they happened on this window.  @off should be used to resume
    normal event-processing.

    The normal XPCE event-processing ensures the window in which
    a mouse-down event occurrs grabs the pointer until the
    mouse-button is released (see also class gesture).

    Most modern window environments do not allow a global grab on the
    pointer.  Any event happening on any xpce window of the same process
    is redirected to this window.

    See also ->focus and ->grab_keyboard.

    @see window-grab_pointer
    @see window->grab_keyboard
    @see display->inform
    @see window->focus

- window->hide
    Hides (put at the back) the frame of the window on its screen.
    Redefines `graphical ->hide`.

- window->initialise: label=[name], size=[size], display=[display]
    Create a window object from its label, size and the display on which is
    has to be displayed.

    This will create a default frame for this window.  When two windows are
    related using `window ->above`, `window->below`, `window->left` or
    `window->right`, both windows will be made part of the same frame.  The
    frame of the receiver of the `window ->above`, etc.  method will be
    destroyed and this window will be made part of the frame of the argument
    window.

    **Defaults**:

    | label    | `untitled`                           |
    | size     | resource defined (see size resource) |
    | display  | @display			      |

    @see class frame

- window->keyboard_focus: graphical*
    Direct keyboard events to the given graphical.

    @see window-keyboard_focus
    @see device->advance
    @see window->focus

- window->left: window|tile
    Similar to ->above, but rhe receiver is placed left of the argument.

    @see topic Window Layout
    @see window->above

- window->position: point
    Position in <-frame.

- window->x: int
    Move graphical horizontally.

- window->y: int
    Move graphical vertically.

- window->move: point
    Class window redefines the inherited placement methods of class
    device to be equivalent to those of class graphical again.
    Class device defines the position to be the origin of the
    coordinate system, while the position of a window is the
    position of the top-left corner as with class graphical.

- window->normalise: on=area|graphical|chain, mode=[{xy,x,y}]
    Try to make `object` visible by scrolling the window.  Object is either
    an area, a graphical or a chain of graphicals.  The area that is made
    visible is the area itself, the area of the graphical or the union of
    the areas of the graphicals in the chain.

    If it is not possible to make the entire area visible, it is ensured the
    top-left corner of the area is visible.

    If `mode` is x, the window is only scrolled horizontally.
    Likewise if `mode` is y the window is only scrolled vertically.
    In all other cases scrolling is performed in both directions.

    @see window->scroll_to
    @see window<-visible

- window->open: [point], display=[display]
    Ensures the window has a <-frame and invokes `frame->open` to realise the
    toplevel window on the given _display_.  Some window systems, notably
    Wayland, ignore the position argument.

    @see frame->open
    @see window<-confirm
    @see window->open_centered
    @see window->create

- window->open_centered: center=[point|frame], display=[display], grab=[bool]
    Equivalent to `window ->open`, but the frame is centered around the
    given point.

    **Defaults**:
    If no point is specified, the window is opened at the center of the
    display.

    @see frame->open_centered
    @see window<-confirm_centered
    @see window->open

- window->pen: 0..
    Thickness of the line surrounding the window.

    **Defaults**: 1

    @see window->border

- window->pointer: point
    Move the pointer to the indicated position, which is in the same
    coordinate system as the graphicals displayed in the window.  The
    pointer needs not be in the window, nor need the indicated coordinate be
    inside the window.

    This method should be used with care.  Users tend not to like jumping
    mice.  It is generally better to place a UI component close to the
    pointer location (see `event <-position`) than to place the UI component
    far away and jump the pointer towards it.

    Class resize_gesture uses it to position the pointer *exactly* on the
    border of a graphical if it was depressed close to it.

    **Diagnostics**: Succeeds without side-effects if the window is not displayed.

    @see graphical->pointer

- window->post_event: event
    Handle events comming from the underlying windowing system.
    This method deals with focus handling, cursor managements,
    etc.  For normal unfocussed events is calls ->event with the
    same event, similar to the ->event routine on normal graphical
    objects.  The distinction makes the function of ->event the same
    on all graphical classes, simplifying code reuse.  Redefinition
    of ->post_event should rarely be necessary.

- window->redraw: [area]
    Redraw the entire window or some area of it.  This method is normally
    invoked automatically if the window is opened on the display or part of
    it is exposed after it has been obscured by other windows.

    PCE is designed to take care of all visual side-effects automatically,
    so the user should never have to invoke methods explicitly.  An
    exception on this rule are the decorations (scrollbars and label) of the
    window.  If these are changed this method needs to be called explicitly
    to update the window.

- window->reset
    Assigns `window <-current_event` to @nil and releases a possible
    event-focus.  Then invokes `visual ->reset` to take care of the
    graphicals displayed on it.

    @see visual->reset

- window->resize
    Invoked from the low-level window interface after the window has
    been resized for any reason.  If <-resize_message is specified,
    this message is executed with the following arguments:

	| @receiver | The window                 |
	| @arg1     | The window (i.e. same)     |
	| @arg2     | The new size of the window |

    The new size is the `area<-size` of the <-area.

    See also <-visible and `dialog->layout`.

- window->resize_message: code*
    Set the variable <-resize_message and executes it if the window has
    already been created.  Arguments:

	| @receiver: | The WIndow                          |
	| @arg1      | Same as @receiver                   |
	| @arg2      | Size object reflecting the new size |

    @see window-resize_message

- window->right: window|tile
    Similar to ->above, but rhe receiver is placed right of the argument.

    @see topic Window Layout
    @see window->above

- window->scroll_vertical: direction={forwards,backwards,goto}, unit={page,file,line}, amount=int, force=[bool]
    Trap message from vertical scrollbar.

- window->scroll_horizontal: direction={forwards,backwards,goto}, unit={page,file,line}, amount=int, force=[bool]
    Traps a scroll-request from the horizontal scrollbar or a
    wheel-mouse.  See class scroll_bar for details.

    If the `force` argument is @on, the scroll actions is performed
    unconditionally.  Otherwise it is only performed if the window
    has the associated scroll_bar object.  This ensures no scrolling
    is performed based on a wheel-mouse if the window has no
    scroll_bar.

    @see window->scroll_vertical
    @see class scroll_bar
    @see scroll_bar-object

- window->scroll_to: point
    Point is in the coordinate system of the graphicals displayed in the
    window.  Makes this point the top-left corner of the window.  Windows
    can scroll both the positive and negative coordinates.  See also
    <-visible.

    @see window->normalise

- window->size: size
    Argument is the desired size in pixels.  This method enlarges the
    requested size with the <->border and offset caused by possible
    scrollbars and then invokes

	`tile ->set: @default, @default, Width, Height`

    Which will change the *desired* size.  The associated frame then decides
    on the actual size.  If the associated frame is already created, only
    the distribution of the windows inside the frame may be changed.  The
    frame itself won't be resized.  The method `frame ->fit` requests the
    frame to resize its contents *and* itself to the most desirable value.

    Various subclasses of window redefined this method to make the unit
    of the size dependent on their font.

    @see window-display_area
    @see frame->fit
    @see window->display_size

- window->typed: event|event_id, delegate=[bool]
    Handle accelerator bindings.  This method is normally invoked from
    ->event if no graphical object in the window accepted the typed key.
    The window will forward the typed key to the frame using `frame ->typed.`
    The frame in turn will check for another window (generally a dialog object)
    that is capable of handling the typed key.

    If delegate = @on (as invoked from ->event) it invokes ->typed on
    <-frame.

- window->uncreate
    Destroy associated window system resources.  This method is used
    internally when a window is deleted from another window or its
    frame.

- window->unlink
    In addition to `device ->unlink`, this method takes care of the
    <-decoration and the associated window-system window.

- window->update_scroll_bar_values
    This message is sent to the window object if either the window
    is resized (see ->resize), the bounding box of the content
    changed (see ->changed_union) or the window was scrolled
    (see ->scroll_to).

    It can be redefined to constrain other objects to these actions.
    The redefined method must call this method to ensure proper
    update of the windows ->scrollbars.

## Get methods {#class-window-get}

- window<-confirm: position=[point], grab=[bool] -> any
    Creates the window and then invokes `frame <-confirm` on its frame.

    @see window->open
    @see window<-confirm_centered
    @see frame<-confirm

- window<-confirm_centered: center=[point|frame], display=[display], grab=[bool] -> any
    Similar to `window <-confirm`, but centers the frame of the window
    around the given position instead of putting the top-left corner at
    the given position.

    @see window->open_centered
    @see frame<-confirm_centered
    @see window<-confirm

- window<-contained_in: -> frame|device
    Implements `visual <-contained_in` for class window.  Windows normally
    appear as tiled objects in a frame object.  They can also be displayed
    as normal graphical objects.

    @see visual<-contained_in

- window<-convert: graphical -> window
    Convert a graphical to it's window using `graphical <-window`.
    Simplifies activating methods that accept a window as argument.

- window<-foreground: -> colour
    Equivalent to <-colour.

    @see window->colour

- window<-frame: create=[bool] -> frame

- window<-visible: -> area
    New area object reflecting the visible area of the drawing plane.  The
    <-position of this area is the coordinate of the top-left corner of the
    window.  Its value is initially (0,0), but may be changed if the window
    is scrolled.  The <-size is the size of the visible area of the drawing
    plane.

    @see window<-display_size
    @see window->normalise

