# class frame {#class-frame}

A `frame` is a top-level window in the desktop environment.  It hosts a
collection of `window` instances and mediates between them and the OS
window manager: it carries the title, decorations, geometry, modality
and close/save handlers.

The frame's interior is tiled: the member windows together cover the
entire client area without overlap.  Layout is managed by a `tile`
object associated with the frame.

Every window needs a frame before it can be opened.  For simple
applications the user does not need to deal with frames explicitly —
opening a single window creates a frame as a side-effect.  Windows
delegate messages they don't understand to their associated frame.

Subclasses of `frame` are common in user-defined applications that
need to coordinate several sub-windows.  The set of windows and their
spatial relationships may be changed after the frame has been opened.

The underlying driver is SDL3 across all platforms; legacy X11
behaviour and window-manager protocol names (WM_DELETE_WINDOW,
WM_SAVE_YOURSELF, ...) are preserved as identifiers but no longer
imply X-specific semantics.

@see window->initialise
@see display<-frames
@see topic Window Layout
@see class tile
@see class window


## Class variables {#class-frame-classvars}

- frame.background: colour|pixmap = @_dialog_bg
    Default background of the frame.  Inherited by windows that do
    not set an explicit background.

- frame.busy_cursor: cursor* = watch (Unix/macOS) / win_wait (Windows)
    Cursor used by `frame ->busy_cursor` when the call site does not
    pass a cursor.

- frame.confirm_done: bool = @off
    Initial value of `<-confirm_done`: whether `->wm_delete` should
    pop up a confirmation dialog.

- frame.geometry: name* = @nil
    When set, determines the default size and position used by
    `->create`/`->open`.  When @nil, the size is determined by the
    contained windows and the position by the OS.

    @see frame->geometry

- frame.can_resize: bool = @on
    Initial value of `<->can_resize`.

- frame.horizontal_resize_cursor: cursor = ew_resize
    Cursor shown over a horizontal subwindow boundary that can be
    dragged to resize the adjacent tiles.

- frame.vertical_resize_cursor: cursor = ns_resize
    Same as the horizontal variant for vertical boundaries.

- frame.fit_after_append: bool = @off
    Automatically run `->fit` after each `->append`.  Convenient
    while interactively assembling a frame; usually left @off in
    production code which builds the layout up-front and then calls
    `->fit` (or `->open`) once.

- frame.decorate_transient: bool = @on
    If @on, transient frames are still given OS decorations (title,
    border).  Set to @off to obtain bare transient windows.


## Instance variables {#class-frame-instvars}

- frame<->name: name
    Name of the frame.  Used by `application<-member` to find frames
    in an application.  Defaults to `<-class_name`.

    See also `<->label`.

- frame<-label: name
    Title shown by the OS in the frame's title bar.

- frame<->application: application*
    Application this frame belongs to.  Combining frames into an
    application lets the application find them by name and define
    modal relations between them.  See `->modal`.

- frame<->display: display
    Display (monitor) hosting the frame.  Defaults to `@display`.
    Changing it after `->create` is not allowed.

- frame<->background: colour|pixmap
    Background of the frame's client area.

- frame<->area: area
    Client-area rectangle (the frame without title-bar and borders)
    in display coordinates.  Reflects the current state regardless of
    what last updated it.

    See also `->set`, `<-bounding_box` and `->geometry`.

    @see frame->set
    @see frame<-size

- frame-geometry: name*
    Pending geometry specification used by `->create` to position
    the frame.  Filled by `->initialise` from the `frame.geometry`
    class variable, or by `->geometry`.  Class `frame` itself does
    not provide a default; subclasses commonly do so end users can
    override the position in `~/.xpce/Defaults`.

    See `library(persistent_frame)` for a frame subclass that
    persists its geometry and sub-window layout.

    For the syntax see `->geometry`.

    @see frame->create
    @see frame->geometry

- frame<-placed: bool
    `@on` once the OS has positioned the frame.  Used internally to
    distinguish an as-yet-unpositioned frame from one whose position
    has been chosen.

- frame-members: chain
    Chain of member windows.  Holds plain windows and windows wrapped
    in a `window_decorator`.  The getter `<-members` returns the
    chain of undecorated windows.

- frame<->kind: {toplevel,transient,popup}
    What this frame is from the desktop's point of view:

    - `toplevel` — ordinary application window: title bar, can be
      minimised and resized through the OS.  Default.
    - `transient` — dialog box / inspector window for a parent
      `toplevel`.  Usually decorated but skipped from the task
      switcher.
    - `popup` — completely invisible to the OS window manager.
      Used for tooltips and popup menus.  Cannot normally receive
      keyboard input; use `transient` if you need text input.

    The kind can only be changed before `->create`.

    @see frame->initialise
    @see frame->transient_for
    @see frame->show_label

- frame<->transient_for: frame*
    Frame for which this frame is a transient (e.g. its parent
    toplevel).  Only meaningful when `<-kind` is `transient`.

    @see frame<-transients
    @see frame->transient_for

- frame<-transients: chain*
    Back-pointer chain holding the frames that are transients of
    this frame.  Maintained by `->transient_for` and `->unlink`.
    Used to propagate state changes (mapped/exposed/hidden) to the
    transients.

    @see frame<-transient_for
    @see frame->transient_for

- frame<->modal: {application,transient}*
    A modal frame must be completed before its context can be used
    again.  By default (@nil) the user may operate on any frame.
    `transient` blocks just the frame in `<-transient_for`;
    `application` blocks every frame in the same `<-application`.

    Typical pattern:

    	display_for(Owner, Result) :-
    		new(D, dialog('Enter information')),
    		send(D, transient_for, Owner),
    		send(D, modal, transient),
    		<fill the dialog>
    		get(D, confirm_centered,
    		    Owner?area?center, Return),
    		send(D, destroy),
    		Return \== @nil.

    See also `<-confirm`, `->transient_for` and class `application`.

- frame<-status: {unlinking,unmapped,hidden,iconic,window,full_screen}
    Current visibility of the frame:

    - `unmapped` — no OS counterpart yet (the frame exists only as
      an xpce object).
    - `hidden` — OS counterpart exists but is not visible.  Used to
      stash a frame for later reuse.
    - `iconic` — minimised.
    - `window` (also written `open`) — fully visible.  Reached via
      `->open`.
    - `full_screen` — maximised across the entire screen.  Reach it
      with `send(F, status, full_screen)` after `->create`.

    See also `->open`, `<-confirm` and `->wait`.  `->show` and
    `->closed` map to this slot for backward compatibility.

- frame<->can_delete: bool
    When `@off`, the frame refuses an OS close request (e.g.
    clicking the close button).  Defaults to `@on`.

    @see frame->wm_delete
    @see frame->wm_protocol

- frame<->can_resize: bool
    When `@on` (default) the OS allows the user to resize the
    frame.  Set to `@off` before `->create` to lock the size.

- frame<->confirm_done: bool
    When `@on`, a confirmation dialog pops up before honouring an
    OS close request (see `->wm_delete`).

    Defaults to the value of the `confirm_done` class variable (off).

    @see frame->wm_delete
    @see frame->wm_protocol
    @see frame<-can_delete

- frame<->input_focus: bool
    `@on` while the frame holds keyboard focus in the OS.

    On change to `@on` the frame sends `window ->input_focus: @on`
    to `<-keyboard_focus` (or to the window under the pointer).  On
    change to `@off` the window that currently owns input focus is
    sent `window ->input_focus: @off`.

    @see frame->keyboard_focus
    @see frame->input_window

- frame<->sensitive: bool
    When `@off` the frame ignores all user input.  Used internally
    by `->busy_cursor` when `block_input` is `@on`.

- frame-return_value: unchecked
    Holds the value passed to `->return` until `<-confirm` picks it
    up.

    @see frame->return
    @see frame<-confirm

- frame<-wm_protocols: sheet
    Sheet mapping protocol names to code objects, used by the
    `->wm_protocol`/`->done_message`/`->save_message` family of OS
    close/save handlers.

    @see frame->wm_protocol
    @see frame->delete_wm_protocol


## Send methods {#class-frame-send}

- frame->initialise: label=[name], kind=[{toplevel,transient,popup}], display=[display], application=[application]
    Create a frame from its label, kind, display and (optional)
    application.  Frames are often created implicitly as a
    side-effect of creating a window.

    Defaults:

    	label		untitled
    	kind		toplevel
    	display	@display

- frame->unlink
    Destroy the frame and all its member windows.  Transients are
    sent `->free`.

- frame->reset
    Cancel any active `->busy_cursor` and restore the normal cursor.

- frame->convert_old_slot: slot=name, value=any
    Backward-compatibility hook used when loading saved frames:
    translates the legacy `show` slot into `<-status`.

- frame->initialise_new_slot: var=variable
    Initialise newly-introduced slots (currently `background`) when
    loading older saved instances.

- frame->display: display
    React to a display change (e.g. when the host moves the frame to
    another monitor).  Applications normally do not call this.

- frame->append: subwindow=window
    Append a window to the frame.  Typical when assembling a frame
    from several sub-windows:

    	:- pce_begin_class(mail, frame, "Simple mail tool").

    	initialise(M) :->
    		"Create mail tool"::
    		send(M, send_super, initialise, mail),
    		/* append the various parts to the tool */
    		send(M, append, new(B, browser)),
    		send(M, append, new(V, view)),
    		send(M, append, new(D, dialog)),
    		/* specify the layout of the parts in the tool */
    		send(V, below, B),
    		send(D, below, V),
    		...

    @see topic Window Layout
    @see window->above

- frame->delete: member=window
    Remove a window from the frame.  See also `->append`,
    `window->left`, `window->above`, ...

- frame->fit
    Recompute the layout of the sub-windows and resize the frame to
    fit the tiled result.  Internally:

    - `tile ->enforce: @off` on the root tile;
    - `window ->_compute_desired_size` on each sub-window (currently
      only `dialog` implements this);
    - `tile ->enforce: @on` on the root tile;
    - `->set` to the root-tile's ideal width/height.

    Normally called from `->create`; user code calls it after
    runtime layout changes (e.g. after `window ->size` or after
    modifying a dialog window).

    @see frame->create
    @see frame->set
    @see dialog->_compute_desired_size
    @see tile->enforce

- frame->resize
    Recompute the layout of the sub-windows in response to the OS
    resizing the frame.

- frame->update_tile_adjusters: [tile]
    Walk the `<-tile` hierarchy and create a `tile_adjuster` object
    for each tile whose `<-can_resize` is `@on`; free adjusters of
    non-resizable tiles.  Called from `->create`.

- frame->set: x=[int], y=[int], width=[int], height=[int], display=[display]
    Modify the client area (without title-bar/borders).  Window
    system constraints may clamp the actual values.

    When the frame is already shown, the request is sent
    asynchronously to the OS; the slots are updated when the OS
    confirms.  A successful size change triggers `->resize`.

    @see frame->size
    @see frame->position
    @see frame->geometry

- frame->size: size=size
    Equivalent to `->set: @default, @default, size?w, size?h`.

- frame->width: width=int
- frame->height: height=int
- frame->x: x=int
- frame->y: y=int
    Convenience wrappers around `->set` that change one axis.

- frame->position: position=point
    Set the top-left position of the frame.  Equivalent to `->set`
    with the point's x and y.

- frame->move: position=point
    Alias for `->position`.

- frame->center: center=[point], display=[display]
    Move the frame so the given point becomes its centre.  Requires
    the frame to be already created.

- frame->area: area
    `->set` from the four fields of the argument area.

- frame->geometry: geometry=name, display=[display]
    Parse the geometry specification and apply it (immediately if the
    frame is open, otherwise stored for `->create`).  The syntax is

    	<width>x<height>[+-]<X>[+-]<Y>[@<monitor>]

    All five components are optional.  A negative X / Y is measured
    from the right / bottom edge of the screen.  Example:

    	400x200+0-0

    means a 400x200 frame anchored at the bottom-left.  The size
    applies to the client area; the position to the frame including
    borders.

    The `@<monitor>` suffix, where `<monitor>` is a value from
    `display_manager <-members`, picks the monitor used as the
    origin.  This syntax follows the FVWM convention.  If the
    `display` argument is given explicitly, `@<monitor>` is ignored.

    @see frame->set
    @see frame.geometry

- frame->create
    Realise the frame in the window system:

    1. Succeed immediately if already created.
    2. Open `<-display` if needed.
    3. Append the frame to `display <-frames`.
    4. `->fit` to lay out sub-windows.
    5. Create the OS window.
    6. `->create` each member window.
    7. Update cursors.
    8. Set the transient-for relationship (if any).
    9. Apply `-geometry` if set.
    10. `->update_tile_adjusters`.

    @see frame->open
    @see display<-frames
    @see frame->fit

- frame->uncreate
    Destroy the OS counterpart, leaving the xpce object intact for
    later reuse.

- frame->open: position=[point], display=[display], grab=[bool]
    Open the frame at the given position:

    1. `->create` if not yet created.
    2. `->set` the position if supplied.
    3. `->status: window`.

    If `grab` is `@on` the frame grabs the pointer (see
    `window->grab_pointer`).

    @see frame->open_centered
    @see frame->set
    @see frame->create
    @see window->open

- frame->open_centered: center=[point|frame], display=[display], grab=[bool]
    Like `->open` but centres the frame on the given point (or the
    centre of `<-display` if omitted).

- frame->show: show=bool
    Show (`@on`) or hide (`@off`) the frame on the display.  When a
    frame becomes shown its transients are shown too.  See also
    `->open` and `->status: hidden`.

- frame->mapped: bool
    Called when the OS reports the frame mapped (`@on`) or unmapped
    (`@off`).  Triggers `->show` on transients to follow the main
    frame.

- frame->wait
    Dispatch events until the frame has been mapped and its windows
    have processed at least one redraw request.  Used to force the
    frame visible during a long computation.  See also `->flush`
    and `->synchronise`.

- frame->status: {unmapped,hidden,iconic,window,full_screen,open}
    Set the frame's visibility status (see `<-status`).  `open` is
    a synonym for `window`.

- frame->closed: open=bool
    Compatibility shortcut: `->closed: @off` opens the frame
    (`->status: window`), `->closed: @on` iconifies it.

    @see frame<-closed
    @see frame->status

- frame->expose
    Raise the frame to the top of the window stack.  Internally
    calls `->closed: @off` (in case the frame was iconic) and then
    asks SDL to raise the OS window.  On success `->exposed` is
    invoked via the resulting OS event.

    @see frame<-postscript
    @see frame->closed

- frame->exposed
    Invoked when the OS reports the frame raised.  Sends `->expose`
    to each transient frame.

    @see frame->hidden
    @see frame<-transients

- frame->hidden
    Invoked when the OS reports the frame hidden (lowered or
    minimised).  Sends `->hide` to each transient frame.

    @see frame->exposed
    @see frame->transient_for

- frame->label: label=name
    Set the title shown in the frame's decorations.  Whether and how
    the label appears is up to the OS theme.

- frame->bell: volume=[int]
    Ring the bell on the display hosting the frame.

    @see display.volume
    @see display->bell

- frame->busy_cursor: cursor=[cursor]*, block_input=[bool]
    Show a temporary cursor (default `frame.busy_cursor`) on every
    sub-window of the frame.  When `block_input` is `@on`, all
    pointer and keyboard events are blocked while the cursor is
    active.  `->busy_cursor: @nil` restores the normal cursor and
    re-enables input.

    @see display->busy_cursor

- frame->cursor: [cursor]
    Set the cursor shown over the frame's decoration / background
    (i.e. when the pointer is on the area between sub-windows).
    Used by `->event` while resizing tiles.

- frame->input_focus: bool
    Receive notification from the window system that this frame has
    (`@on`) or has lost (`@off`) keyboard focus.  Forwards to
    `<-keyboard_focus` or the window currently under the pointer.

- frame->input_window: window
    Direct all keyboard events arriving on this frame to the
    indicated window.  Appropriate when only one sub-window contains
    keyboard-sensitive controls.

- frame->keyboard_focus: [window]*
    When set to a window, all keyboard input arriving at any
    sub-window is redirected there.  The argument need not be a
    member of the frame.  `@nil`/`@default` makes keyboard input
    follow the pointer.

- frame->event: event
    Handle an event that landed on the frame's background or
    decoration.  Two cases are dispatched:

    - keyboard event with an `<-input_focus` window: forward to that
      window;
    - pointer on a sub-window boundary: indicate / perform a resize
      drag.

- frame->post_event: event
    Re-dispatch a keyboard event that wasn't consumed elsewhere.
    Fails by default.

- frame->typed: event|event_id
    Distribute a typed key across the frame's sub-windows so any
    window can accept it as an accelerator.

- frame->return: unchecked
    Make a blocking `<-confirm` call on this frame return with the
    given value.  Stores it in `-return_value`.

    @see frame<-confirm
    @see frame-return_value

- frame->report: kind={status,inform,progress,done,warning,error,fatal}, format=[char_array], argument=any ...
    Redefinition of `visual ->report` that walks a frame's report
    chain:

    1. Call `<-report_to`; if it returns a value other than
       `<-display`, forward `->report` there.
    2. Try `->report` on each `<-members` window until one
       succeeds.  Windows by default delegate back to their frame,
       so the loop detects and breaks this cycle.
    3. If the frame is transient, forward to `<-transient_for`.
    4. Fall back to `visual ->report`.

- frame->wm_protocol: protocol=name, action=code
    Register a handler for an OS protocol message.  The name is
    matched against incoming protocol events; on a match the code
    is executed with `@receiver` set to the frame and `@arg1` to
    its first window.

    Historically these names matched X11's WM_PROTOCOLS messages,
    notably `WM_DELETE_WINDOW` (close request) and
    `WM_SAVE_YOURSELF` (session save).  On SDL3 only the
    close-request path remains; the others are accepted but never
    triggered.

    @see frame<-wm_protocols
    @see frame->save_message
    @see frame->done_message
    @see frame->delete_wm_protocol

- frame->delete_wm_protocol: protocol=name
    Remove a protocol handler installed with `->wm_protocol`.

- frame->done_message: action=code
    Shortcut for `->wm_protocol: 'WM_DELETE_WINDOW', action`.
    Default: `message(@receiver, wm_delete)`.

    @see frame->wm_delete
    @see frame->wm_protocol

- frame->save_message: action=code
    Shortcut for `->wm_protocol: 'WM_SAVE_YOURSELF', action`.
    Kept for completeness; SDL3 does not emit this event.

- frame->wm_delete
    Default action for an OS close request:

    1. Fail if `<-can_delete` is `@off`.
    2. If `<-confirm_done` is `@on`, pop up
       `display ->confirm: 'Delete window "%s"'?', <-name`.  Fail
       if the user cancels.
    3. `->destroy` the frame.

    @see frame->done_message
    @see frame<-can_delete
    @see frame<-confirm_done

- frame->show_label: show=bool
    Compatibility alias: `@on` is equivalent to `->kind: toplevel`,
    `@off` to `->kind: transient`.  New code should use `->kind`.

- frame->transient_for: frame*
    Declare this frame as a transient (e.g. dialog or inspector) for
    the argument frame.  Effects:

    - The OS is told about the relationship so it can stack and
      iconify the transient with its parent.
    - When the parent is destroyed, mapped, exposed or hidden, the
      same action is propagated to its transient frames.

    @see frame->unlink
    @see frame<-transient_for
    @see frame<-transients

- frame->attach_transient: frame
- frame->detach_transient: frame
    Maintain the `<-transients` chain.  Called by `->transient_for`
    and `->unlink`; user code rarely invokes them directly.

- frame->redraw: [area]
    Redraw the resize-handle widgets between sub-windows.  Used
    after layout changes.


## Get methods {#class-frame-get}

- frame<-frame: -> frame
    Returns itself.  Provided so the same selector resolves on a
    frame, on its member windows and on the graphicals displayed
    within.

- frame<-contained_in: -> display
    Equivalent to `<-display`.

- frame<-contains: -> chain
    Chain with all member windows; equivalent to `<-members`.

- frame<-members: -> chain
    New chain holding the (undecorated) member windows.

- frame<-member: name -> window
    First member window with that name.  Looks through the
    `window_decorator` wrappers automatically.

- frame<-catch_all: window_name=name -> window
    Maps `<window_name>_member` to the corresponding member window:

    	?- new(@f, frame),
    	   send(@f, append, new(V, view)),
    	   send(new(D, dialog), below, V),
    	   send(V, open).

    	?- send(@f?view_member, format, 'Hello World\n').

- frame<-convert: window -> frame
    Convert a window to its containing frame.

- frame<-tile: -> tile
    Root of the tile hierarchy that lays out this frame.

    @see class tile

- frame<-area: -> area
    Client-area rectangle of the frame on the display.

- frame<-size: -> size
    Size of `<-area`.

- frame<-position: -> point
    Top-left position of `<-area` on the display.

- frame<-geometry: -> name
    Geometry specification for the current state of the frame,
    suitable for storing and re-applying via `->geometry`.

- frame<-show: -> bool
    `@on` when `<-status` is `open`; `@off` otherwise.

- frame<-closed: -> bool
    `@on` when the frame is iconified, `@off` when it is open.

- frame<-image: [{bitmap,pixmap}] -> image
    Image holding the pixels currently shown by the frame.

- frame<-keyboard_focus: -> window
    Window currently designated to receive keyboard input.

- frame<-confirm: position=[point], display=[display], grab=[bool] -> return_value=any
    Open the frame (if not yet opened) and block until `->return` is
    invoked.  Used to implement modal prompts.

    When `grab` is `@on` the frame grabs the pointer.  If the frame
    is freed while the confirmer is blocking, the call fails.

    Typical use:

    	ask_name(Name) :-
    		new(D, dialog('Name Prompter')),
    		send(D, append, new(N, text_item(name, ''))),
    		send(D, append,
    		     button(ok, message(D, return, N?selection))),
    		send(D, append,
    			 button(cancel, message(D, return, @nil))),
    		send(D, default_button, ok),
    		get(D, confirm, Answer),
    		send(D, destroy),
    		Answer \== @nil,
    		Name = Answer.

    @see frame->return
    @see frame-return_value
    @see frame<-confirm_centered
    @see window<-confirm

- frame<-confirm_centered: center=[point|frame], display=[display], grab=[bool] -> return_value=any
    Like `<-confirm` but centres on `center` (or on the screen if
    omitted) rather than placing the top-left there.

- frame<-open_file: filters=[chain], default=[char_array], allow_many=[bool] -> name|chain
    Use the OS file-open dialog to prompt for a file.  Arguments:

    - `filters` — chain of `tuple` objects, each `tuple(Name,
      Extensions)` where `Extensions` is a `;`-separated list or a
      chain of char_array.  Example:

    	chain(tuple('Prolog', chain(pl,prolog))).

    - `default` — absolute file name used as initial selection.
    - `allow_many` — if `@on`, allow multi-select and return a chain
      of file names; otherwise return a single name.

- frame<-save_file: filters=[chain], default=[char_array] -> name
    Use the OS file-save dialog.  Arguments as for `<-open_file`
    minus `allow_many`.
