# class display {#class-display}

Class `display` is xpce's handle on the host's graphical output: a
physical monitor, the system theme, system clipboard and the
collection of frames currently hosted on that monitor.  The underlying
driver is SDL3 across all platforms.

xpce creates an instance for each monitor when the window system is
initialised.  Additional `display` instances appear dynamically when a
hotplugged monitor is attached (and disappear on removal, except
for the last one; see `display->removed`).  Most
applications use `@display`, the function that evaluates to the
display that received the last event or else the primary display.
See `display_manager <-current`.

@see display<-convert
@see display<-primary
@see class display_manager


## Class variables {#class-display-classvars}

- display.background: colour = ui_window_background
    Default background for newly created windows.

- display.foreground: colour = ui_window_foreground
    Default foreground for newly created windows.

- display.label_font: font = bold
    Font used for the prompt of `display ->inform` and
    `display ->confirm`.  Companion: `display.value_font`.

- display.value_font: font = normal
    Font used for the body of `display ->inform` /
    `display ->confirm`.

- display.theme: [name] = @default
    The SWI-Prolog theme for the xpce user interface and the
    development tools, e.g., `dark`.  If @default, the theme follows
    `<-system_theme`, also if this changes while xpce is running.
    Set this in the Defaults file to select a theme permanently.  The
    Settings/Theme menu of the IDE sets it for the running session.
    See `library(pce_theme)`.

- display.volume: 0..100 = 0
    Default volume for `display ->bell`.


## Instance variables {#class-display-instvars}

- display<->name: name
    Human-readable name reported by the OS for this monitor.

- display<-number: int
    Numeric identity assigned to the monitor; stable across the life
    of the process and used by the geometry syntax (e.g.
    "+0+0:1" picks monitor 1).

- display<->primary: bool
    `@on` when this display is the OS-designated primary monitor.
    Exactly one display has this set at any time.

- display<-area: area
    Pixel rectangle occupied by this monitor in the global desktop
    coordinate space.

- display<-work_area: area
    Subrectangle of `<-area` available for application windows (the
    rest is reserved by the desktop, e.g. for taskbars and docks).

- display<-frames: chain
    Chain of all frames currently hosted on this display.  Maintained
    by `frame ->create` and `frame ->unlink`; frames that have not
    been created yet do not appear here.

    @see frame->create
    @see class frame

- display<-display_manager: display_manager
    The global display manager (@display_manager).

    @see class display_manager

- display<->foreground: colour
    Initial foreground colour for newly created windows.  Changing
    this does not affect existing windows.

- display<->background: colour
    Initial background colour for newly created windows.  Changing
    this does not affect existing windows.

    @see window->background

- display<-inspect_handlers: chain
    Chain of handler objects used to support debugging tools.  This
    chain is shared by all displays: it is `display_manager
    <-inspect_handlers`.  When an
    event occurs on a window and this chain is non-empty, the handlers
    in this chain are tried before the normal event-handling
    procedure.

    xpce searches for the deepest nested graphical that overlaps the
    event-position.  A possible event focus is ignored.  If the event
    matches the type of the handler, the message of the handler is
    executed with the following argument binding:

    | @receiver	| The graphical |
    | @arg1	| Idem		|
    | @arg2	| The event     |

    If this message succeeds, the event is considered to be dealt with.

    @see tool Inspector
    @see display->inspect_handler


## Send methods {#class-display-send}

- display->initialise: name=name, area=area
    Create a display for the monitor with the given OS-reported name
    and area.  `@display` and any additional hotplugged displays are
    created automatically; applications normally do not call this.

- display->unlink
    Detach the display from `@display_manager`.  Invoked by the
    display-manager when the monitor is removed.

- display->removed
    Mark the display as hotplug-removed and move its frames to the
    display the window system reports for their window, or else to
    `display_manager <-primary`.  The display is freed when it has no
    frames left.  If it was the last display it is kept as a parking
    place: frames stay on it and new frames open on it, so `@display`
    does not fail.  When a monitor is attached again, the frames move
    to it and the parked display is freed.

- display->poll_dimensions
    Re-query the OS for the monitor's `<-area` and `<-work_area`.
    Called automatically on display events; can be invoked
    to refresh after an external change.

- display->bell: volume=[0..100]
    Ring the bell on this display.  Volume defaults to
    `display.volume`.

    @see display.volume
    @see frame->bell
    @see graphical->bell

- display->busy_cursor: cursor=[cursor]*, block_input=[bool]
    Define (temporary) cursor for all frames on the display by calling
    `frame ->busy_cursor` on every entry in `<-frames`.  Used by
    `popup ->execute` and `click_gesture ->terminate`.  Use
    `display_manager ->busy_cursor` for all frames of the application.

- display->confirm: for=[visual], title=[char_array], message=char_array, any ...
    Format a string from the arguments and display a modal dialog
    asking the user to confirm.  Returns success on confirm, failure
    on cancel.

    @see pce->confirm
    @see string->format
    @see display->inform

- display->inform: for=[visual], title=[char_array], message=char_array, any ...
    Display a modal information dialog and wait for the user to
    dismiss it.  The first argument is a format string; the remaining
    arguments are substituted via `string ->format`.  See also
    `pce ->format` and `visual ->report`.

    @see pce->inform
    @see string->format
    @see graphical->alert
    @see display->confirm

- display->report: kind={status,inform,progress,done,warning,error,fatal}, format=[char_array], argument=any ...
    Catch-all for `->report` messages on visual objects.  `inform`
    and `error` invoke `->inform`; `error` and `warning` also send
    `graphical ->alert` to @reportee.  Status reports are ignored.

- display->dispatch
    Invoke `display_manager ->dispatch` to dispatch events for about
    1/4 of a second.

    @see display_manager->dispatch

- display->event_queued
    Succeeds if there are events waiting in the OS queue for this
    display.  Does not handle them.

- display->copy: char_array
    Place the argument on both the primary selection and the system
    clipboard.

- display->selection: which=[name], value=char_array
    Become owner of the selection (`primary` or `clipboard`) with the
    given textual value.

- display->screen_saver: bool
    Activate (`@on`) or inhibit (`@off`) the OS screensaver while
    xpce is running.

- display->screen_keyboard: [{auto,on,off}]
    Policy for showing the on-screen keyboard when a text-input
    widget gains focus.  `auto` lets SDL decide based on whether a
    physical keyboard is attached.

- display->inspect_handler: handler
    Add a handler to `<-inspect_handlers` (using `chain ->add`).  As
    this chain is shared, the handler applies to all displays.

    @see topic Finding References
    @see display<-inspect_handlers

- display->has_visible_frames: keep_alive=[bool]
    Succeeds if at least one frame on this display is currently
    visible.  If `keep_alive` is `@on`, only frames whose
    `frame <-keep_alive` is `@on` count.

    @see frame<-keep_alive


## Get methods {#class-display-get}

- display<-contained_in: -> display_manager
    Implements `visual<-contained_in` protocol.

- display<-contains: -> chain
    Implements `visual<-contains` protocol.

- display<-convert: any -> display
    Convert a graphical to the display it is shown on, or accept an
    already-existing display object.

- display<-depth: -> bits_per_pixel=int
    Bits used to represent a display pixel.  Returns 32 (RGBA) on all
    current platforms; preserved for compatibility with code that
    branched on colour depth.

- display<-size: -> size
    Pixel size of the display: equivalent to `<-area?size`.

- display<-width: -> int
    Pixel width of the display: equivalent to `<-area?width`.

- display<-height: -> int
    Pixel height of the display: equivalent to `<-area?height`.

- display<-system_theme: -> {light,dark}
    The OS system theme as reported by `SDL_GetSystemTheme()`.  On
    Windows, a contrast theme with a dark window background also
    counts as `dark`.  Fails if the platform cannot tell.

- display<-theme: -> name
    The SWI-Prolog theme for the xpce user interface: the class
    variable `display.theme` or, if that is @default, `<-system_theme`.
    `library(pce_theme)` applies this theme when xpce starts and, if
    the class variable is @default, when the desktop switches between
    light and dark.

- display<-paste: which=[{primary,clipboard}] -> string
    Return the textual content of the indicated selection (defaults
    to `clipboard`).  Pair with `->copy` and `->selection`.

    @see display->copy
    @see display<-selection

- display<-selection: which=[name], target=[name], type=[type] -> any
    Query the value of a system selection.  `which` defaults to
    `primary`; common alternatives are `clipboard` and `secondary`.
    `target` is the requested MIME-like type (default `text`;
    `utf8_string` for explicit UTF-8).  `type` is the xpce type the
    returned value should be coerced to via `type <-check`.

    @see display<-paste

- display<-has_screen_keyboard_support: -> bool
    `@on` when SDL reports the platform has an on-screen keyboard.

- display<-screen_keyboard_shown: -> bool
    `@on` when the on-screen keyboard is currently visible.
