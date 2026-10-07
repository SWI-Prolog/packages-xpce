# class display_manager {#class-display_manager}

Class `display_manager` has a single instance `@display_manager`,
created at boot time.  Its responsibilities are:

- Maintain the set of attached `display` objects (`<-members`,
  `<-member`, `<-primary`, `<-current`).  On SDL3 the set is updated
  dynamically as monitors are added or removed.
- Drive the top-level event loop (`->dispatch`) and flush damage to
  the screen (`->redraw`).
- Reload the system colours if the desktop settings change and
  redraw all windows after colours changed (`->system_colours_changed`,
  `->colours_changed`).

@see class display
@see class theme_colour
@see display<-display_manager


## Instance variables {#class-display_manager-instvars}

- display_manager<-members: chain
    Chain of currently attached `display` objects, one for each
    monitor.  Gains and loses entries as monitors are hotplugged.

- display_manager<->test_queue: bool
    When `@on`, redraw passes are interrupted whenever events become
    available, yielding faster perceived response while typing or
    dragging.  Default is `@on` everywhere.

- display_manager<->focus_message: code*
    Optional code object invoked with the frame that gained keyboard
    focus.  Used by tools (e.g. the symbol picker) to track the
    active window without polling.

- display_manager<->system_colours_message: code*
    Optional code object that is executed by
    `->system_colours_changed` after reloading the system colours and
    before redrawing.  `library(pce_theme)` uses this to select the
    theme that matches the new desktop settings.

- display_manager<-inspect_handlers: chain
    Handlers that support inspector tools.  The chain is shared by all
    displays: it is also `display <-inspect_handlers` of each display.

    @see display<-inspect_handlers


## Send methods {#class-display_manager-send}

- display_manager->initialise
    Create the manager.  Called once at boot to build
    `@display_manager`; not used directly.

- display_manager->append: display
    Attach a new display to the manager.  Called by `display`'s
    constructor; applications normally do not invoke this.

- display_manager->redraw
    Flush all pending changes to the screen.  Called from the
    top-level event loop to repaint windows queued in
    `@changed_windows`; may be overridden for special event-loop
    hooks.

    @see display->flush
    @see display->synchronise
    @see graphical->compute

- display_manager->has_visible_frames: keep_alive=[bool]
    Succeeds if any attached display has at least one visible frame
    (used to decide when the application can exit).  If `keep_alive`
    is `@on`, only frames whose `frame <-keep_alive` is `@on` count.

    @see display->has_visible_frames
    @see frame<-keep_alive

- display_manager->inspect_handler: handler
    Add a handler to `<-inspect_handlers`.  It applies to all
    displays.

    @see display->inspect_handler

- display_manager->busy_cursor: cursor=[cursor]*, block_input=[bool]
    Send `display ->busy_cursor` to all displays, i.e., define a
    (temporary) cursor for all frames of the application.  Calls must
    be balanced: `@nil` restores the cursor.

- display_manager->system_colours_changed
    Reload the system colours (`sys_*` and the platform specific
    names) from the desktop settings.  Colour objects of these names
    change their value in place and the theme colours are resolved
    again.  Next, run `<-system_colours_message` and redraw all
    windows if anything changed.  xpce sends this message if it is
    told that the desktop settings changed, e.g., if the user switches
    between light and dark mode.  An application may send it as well.

    @see class theme_colour

- display_manager->fonts_changed
    Reload all fonts and recompute and redraw all windows of all frames.
    Send this after changing `font.scale` or `font.pango_families` at
    runtime.  Font objects keep their identity: only their size and
    Pango family change, so all their users see the change.

    Everything that shows text is recomputed, dialogs are laid out
    again and frames resize their windows.  A frame or graphical whose
    class defines `->fonts_changed` is sent this message, so it can drop
    what it computed from the font metrics.  A graphical gets this after
    its contents were recomputed.  Classes editor and menu_bar define it.

- display_manager->colours_changed
    Redraw all windows of all frames, including windows inside other
    windows or devices such as tabs.  Send this after changing colours
    in place, e.g., after changing the value of theme colours.

    Before redrawing, each frame and each graphical in these frames
    whose class defines `->colours_changed` is sent this message.  The
    built-in classes do not define it.  An application defines it if
    it holds colours that do not follow the theme by themselves, e.g.,
    images that are drawn using `image ->draw_in` or colours that are
    computed from other colours:

    	colours_changed(W) :->
    	    "Draw the icons again in the new colours"::
    	    send(W, paint_icons).


## Get methods {#class-display_manager-get}

- display_manager<-contains: -> chain
    Equivalent to `<-members`: chain holding every currently attached
    `display`.

- display_manager<-primary: -> display
    The display the OS designates as primary.  Falls back to the
    first member when no primary is set.  Removed displays (see
    `display->removed`) are only returned if there is no other
    display.

- display_manager<-current: -> display
    The display that received the last event, or `<-primary` when
    no event has happened yet.  Use this when you need to open a
    new frame "where the user is".  `@display` is the function
    `?(@display_manager, current)`.

- display_manager<-frames: -> chain
    New chain holding the frames of all displays.

- display_manager<-member: name|1.. -> display
    Look up a display by its `<-name` or `<-number`.

- display_manager<-window_of_last_event: -> window
    Find the window object that received the last event.  Fails if
    that window has been destroyed.  Used internally to choose the
    first window to repaint; available for similar scheduling tasks
    in user code.
