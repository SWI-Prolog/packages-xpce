# class visual {#class-visual}

Class `visual` is a superclass of all classes who's instances normally
causes a direct visual effect on the display.  Of course this entails
the classes graphical and window.  In addition it entails classes such
as dict_item (which is visible in a browser) and display (which
represents the entire screen).  Class visual serves two purposes:

- Define the consists-of hierarchy of visual objects.
	The methods `Visual <-contained_in` and `Visual <-contains`
	define a hierarchy of visual objects.  This hierarchy allows one
	to examine the `visual world` of PCE and to determine relations
	between visual objects.  This hierarchy is used by the `Visual
	Hierarchy' browser of the manual tools.

- Provide a common place for additional methods.
	For the implementation of some user interface management system
	on top of PCE, class visual provides one place where central
	methods operating on visual objects can be defined.  Attaching
	methods to this class will (through normal PCE subclass
	inheritance) make the method available to any visible object.

The methods `Visual <-contained_in` and `Visual <-contains` actually are
empty.  Each subclass of visual provides the appropriate definitions.

Note that the display_manager object @display_manager is the root of the
consists-of hierarchy.

@see tool Visual Hierarchy


## Send methods {#class-visual-send}

- visual->contained_in: visual
    Succeeds if I'm contained in the argument visual object.  This method
    walks up the visualisation contained hierarchy using `Visual
    <-contained_in'.  A visual is defined to be contained in itself.

- visual->destroy
    Invokes ->free on the visual and all parts of this visual that are not
    protected.  This method may be used to destroy an entire tree of
    visual objects.

    First, it collects all visuals that are part of it and not
    protected or part of a protected visual using `visual <-contains`.  Then
    it will send free to all collected visuals.  This method takes care of
    the fact that freeing one visual might result in the destruction of
    others.

    @see object->free
    @see visual<-contains
    @see object->protect
    @see window->destroy

- visual->report: kind={status,inform,progress,done,warning,error,fatal}, format=[char_array], argument=any ...
    The method ->report provides a general mechanism to present short
    descriptions of status, warnings or errors to the application user. This
    method is integrated with the general error reporting mechanism (see
    `object ->error` and class error.

    If no visual object is available to send the report to, `object ->report`
    may be used.

    The first argument describes the type of the message to be reported.
    The type is an indication on how `serious` the message is and thus
    how much attention it should attract from the application user. Its
    values are:

    - status
    	Information on progress, status change of the system, etc.  such
    	information should not attract much attention. By default, PCE
    	will format the message at an appropriate location.  If no
    	such location can be found the message is ignored

    - inform
    	The user requested information and this is the reply. Handled as
    	`status`, but if no appropriate location can be found it will
    	invoke `display ->inform`.

    - progress
    	Indicates progress in ongoing computation.  Feedback is
    	generally similar to `status`, but followed by a
    	`graphical ->flush` to take immediate effect.

    - done
    	Terminates a (sequence of) ->report: progress messages.

    - warning
    	The user probably made a minor mistake for which a simple alert
    	such as provided by `graphical ->alert` suffices. If there is an
    	appropriate location for the message it will be formatted there.

    - error
    	Something serious went wrong and the user needs to be informed
    	of this.  For example a file could not be read or written.

    The remaining arguments are a format specification and format arguments.
    See `string ->format` for details on PCE's formatting capabilities.

    The method `visual ->report` performs the following steps:

    1. Ask for a visual that might be able to report the message
    	using `visual <-report_to`.

    2. If such an object exists *and* @reportee equals @nil, bind
    	@reportee to the receiver of the ->report message.  @reportee
    	thus points to the original visual object that received the
    	->report message.

    3. Invoke ->report with the same argument on the object.

    The method `visual ->report` is redefined by various subclasses. The
    most important are: `label->report`, `frame ->report` and `display
    ->report'.

    @see object->report
    @see @reportee
    @see object->error
    @see string->format
    @see graphical->alert

- visual->reset
    This method sends ->reset to all visual objects it contains
    (recursively).  `Visual ->reset` is automatically invoked from the
    host-language after an abort to the hostlanguage toplevel.  Various
    subclasses of visual implement this method to reset parameters such as
    status, event-focus, etc.

    **Bugs**:
    Recovery after a crash or abort is not very save.  The consistency of
    the objectbase may be violated, gesture objects often cannot be tracked
    and reset, etc.

    @see window->reset


## Get methods {#class-visual-get}

- visual<-contained_in: -> visual
    Returns the `parent` visualiser.  Defined at each of the various
    subclasses of visualiser to deal with the different implementations.
    All visualiser classes define this method.  The object
    @display_manager is the root of the hierarchy.

    See also <-contains.  Using the manpce/0 window, the menu
    Tools/Visual hierachy provides a tool for inspecting the actual
    hierarchy.

    @see frame<-contained_in
    @see window<-contained_in
    @see visual<-contains

- visual<-container: class|code -> condition=visual
    Find the innermost graphical in the consists-of hierarchy
    defined by class visual and its sub-classes that satisfies
    the given condition. Arguments:

    	@arg1		Visual object considered

    Providing a class argument is a shorthand for

    	message(@arg1, instance_of, <class>).

    See also `<-contains`, `<-contained_in`, `graphical <-window`,
    `graphical <-frame` and `graphical <-display`.

- visual<-contains: -> chain
    Returns a chain with the visualisers of the next level of the contains
    hierarchy.  Each of the subclasses that defines a `compound` visualiser
    defines this method to deal with it's particular implementation.

    @see frame<-contains
    @see visual->destroy
    @see device<-contains
    @see visual<-contained_in

- visual<-frame: -> frame
    Frame I'm part of (if present).  This method uses <-contained_in
    to walk up the consists-of hierarchy of graphical objects until
    it finds and instance of class frame.  Fails silently if no
    frame can be found, which is the case if the receiver is not
    displayed, is a display object or @display_manager.

    See also <-container, `graphical <-window` and `graphical <-device`

- visual<-master: -> visual
    Returns the `stand-alone` visual of this visual.  This definition is
    vague and its implementation is equally ill defined.  The idea is
    illustrated in the view <-> editor example:

    If the programmer creates a view object, s/he creates a specialised
    window with an editor displayed inside it.  The view delegates to its
    editor object and the programmer does not have to be aware that the
    view consists of multiple objects.  Class editor however sometimes
    invokes associated messages (for example `editor <->error_message`.
    We would like to retain the invisibility of the multiple objects by
    binding @receiver to the view rather than to the editor itself.

    The <-master method is a (bad, but we needed something) attempt to solve
    this problem.  It knows about some of these combinations and will return
    the proper `main` object.

    **Defaults**: Visual objects that have not redefined this method return `<-self`

    **Bugs**:
    This method should not be there.  As soon as a better solution to this
    `composite object` problem is found we will replace this.

- visual<-report_to: -> visual
    Object capable of handling a ->report.  By default this is the same as
    <-contained_in.

    @see dialog<-report_to

