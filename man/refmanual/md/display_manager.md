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
    Chain of currently attached `display` objects.  Normally holds
    just `@display` on a single-monitor setup; gains and loses
    entries as monitors are hotplugged.

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

- display_manager->system_colours_changed
    Reload the system colours (`sys_*` and the platform specific
    names) from the desktop settings.  Colour objects of these names
    change their value in place and the theme colours are resolved
    again.  Next, run `<-system_colours_message` and redraw all
    windows if anything changed.  xpce sends this message if it is
    told that the desktop settings changed, e.g., if the user switches
    between light and dark mode.  An application may send it as well.

    @see class theme_colour

- display_manager->colours_changed
    Redraw all windows of all frames, including windows inside other
    windows or devices such as tabs.  Send this after changing colours
    in place, e.g., after changing the value of theme colours.


## Get methods {#class-display_manager-get}

- display_manager<-contains: -> chain
    Equivalent to `<-members`: chain holding every currently attached
    `display`.

- display_manager<-primary: -> display
    The display the OS designates as primary.  Falls back to the
    first member when no primary is set.

- display_manager<-current: -> display
    The display that received the last event, or `<-primary` when
    no event has happened yet.  Use this when you need to open a
    new frame "where the user is".

- display_manager<-member: name|1.. -> display
    Look up a display by its `<-name` or `<-number`.

- display_manager<-window_of_last_event: -> window
    Find the window object that received the last event.  Fails if
    that window has been destroyed.  Used internally to choose the
    first window to repaint; available for similar scheduling tasks
    in user code.
