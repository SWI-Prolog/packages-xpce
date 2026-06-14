# class display_manager {#class-display_manager}

Class `display_manager` has a single instance `@display_manager`,
created at boot time.  Its responsibilities are:

- Maintain the set of attached `display` objects (`<-members`,
  `<-member`, `<-primary`, `<-current`).  On SDL3 the set is updated
  dynamically as monitors are added or removed.
- Drive the top-level event loop (`->dispatch`) and flush damage to
  the screen (`->redraw`).

@see class display
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

- display_manager->has_visible_frames
    Succeeds if any attached display has at least one visible frame
    (used to decide when the application can exit).


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
