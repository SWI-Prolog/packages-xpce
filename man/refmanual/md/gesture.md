# class gesture {#class-gesture}

A gesture is a recogniser object for a sequence of events, starting with
a (mouse-) button down event up to the corresponding up-event.

Class gesture itself takes care of detecting the appropriate down event
that matches the specified modifier object.  Next, it sets the event
focus using `window ->focus` on the window on which the event occurred to
itself and maintains this focus until the corresponding up-event is
detected.  As the execution of a specific gesture is often indicated to
the user by using some cursor object, class gesture allows for the
specification of a cursor during the execution of the event.

Class gesture itself is intended to act a a super-class over classes
that handle particular gestures such as moving, resizing, linking
graphicals, clicking, etc.  The predefined subclasses are:

- class click_gesture
	Deals with clicking objects.  Handles double and triple
	clicks.

- class move_gesture and class move_outline_gesture
	Deals with moving graphicals.

- class resize_gesture and class resize_outline_gesture
	Deals with resizing graphicals

- class connect_gesture
	Deals with connecting graphicals (see `graphical ->connect`).

- class popup_gesture
	Deals with displaying popup menus.  See class popup.


A gesture sends 4 types of messages to itself for which a sub-class of
gesture must define appropriate behaviour

- ->verify: event
	Called after a down-event of the appropriate button with the
	appropriate modifier is detected.  It should succeed if the
	condition to actually start this gesture are satisfied.  Normally
	used to perform checks on the mode of the tool or
	represented object, etc.  Class gesture itself defines a
	verify behaviour that succeeds without side-effects.

- ->initiate: event
	Called right after the ->verify succeeded.  It should perform
	the necessary initialisations for starting the gesture.  If the
	class-variable `cursor` is not @default, the cursor will
	be set automatically.

- ->drag: event
	Called on each mouse-motion event that is received.

- ->terminate: event
	Called on the corresponding up-event.  It should terminate the
	gesture.  The cursor is automatically reset to its original
	value.

@see window-focus_recogniser
@see class modifier
@see window->focus
@see class graphical
@see class event
@see class handler
@see topic Gestures


## Class variables {#class-gesture-classvars}

- gesture.button: button_name = left
    Defines the button for which the gesture is sensitive.  This is the
    normal way to specify the button.

    @see gesture->initialise

- gesture.cursor: [cursor] = @default
    Defines the name of the X-cursor to be used during activation of the
    gesture.

- gesture.modifier: modifier =
    Defines the setting of the shift-, control- and meta-keys demanded by
    the gesture.

    @see modifier<-convert
    @see class modifier
    @see gesture->initialise


## Instance variables {#class-gesture-instvars}

- gesture<-button: button_name
    Name of the button for which the gesture is sensitive.  The filler is
    (currently) one of `left`, `middle` or `right`.  Mouse-events associated
    with any other button are always ignored by the gesture.

- gesture<->condition: code*
    When non-nil, this message is executed on a mouse-button-down-event of
    the appropriate button with the appropriate modifiers.  When executed,
    the following bindings apply:

    	| @receiver: | The gesture          |
    	| @arg1      | Equivalent to @event |

    If this message fails, the gesture is not activated.

    **Defaults**: @nil

- gesture<->cursor: [cursor]
    Cursor object displayed during the activation of the gesture.  Gestures
    that may use multiple cursors (for example the built-in resize_gesture)
    may set this variable in their ->initiate method or set this variable to
    @default and invoke `graphical ->focus_cursor` in ->initiate.

    @see graphical->focus_cursor

- gesture<->modifier: modifier
    Modifier object that describes the position of the shift, control and
    meta keys.  The gesture will only be initiated if the setting of these
    keys match this modifier at the time of the button-down event.

    @see class modifier

- gesture<->status: {active,inactive}
    Status of the gesture.  When `active`, the gesture is initiated and
    processing events.

    @see gesture->event


## Send methods {#class-gesture-send}

- gesture->cancel: event
    Cancel this gesture and try the next one.  This method
    implements the necessary event-focus consequences.  It is
    currently used by class click_gesture.  See `click_gesture ->cancel`
    and `click_gesture <-max_drag_distance`.

    It performs the following steps:

    - Set ->active: @off
    - Set `window ->focus: @nil` of the associated window
    - Resent the original down event to the window
    - Resent the current (drag) event to the window
    - Set ->active: @on

- gesture->drag: event
    This method is send by ->event if the <-status attribute is `active` and
    the event is a drag event (i.e. the mouse is moved with one of the
    buttons held down).

    `gesture ->drag` just succeeds.  Intended to be redefined by gesture's
    subclasses.

    @see gesture->event

- gesture->event: event
    Accepts the following events:

    - ms_{left,middle,right}_down
    	If the position of shift-, control- and meta-keys match the
    	<->modifier, the button matches <->button, the <->condition
    	message is @nil or executed successfully and `gesture ->verify`
    	succeeds, the gesture will perform the following steps:

    1. invoke ->initiate: Event on itself to initiate
    2. set <-status to `active`
    3. invoke `window ->focus` to grab the event-focus

    - ms_{left,middle,right}_drag
    	If <-status equals `active`, invokes ->drag: event to itself

    - ms_{left,middle,right}_up
    	If <-status equals `active` and the button matches the <->button
    	of the gesture, invoke ->terminate on itself and set <->status
    	to inactive.

    Fails when `recogniser <->active` equals @off.

    @see gesture->drag
    @see gesture->initiate
    @see gesture->terminate
    @see gesture->verify
    @see gesture-status

- gesture->initialise: button=[button_name], modifier=[modifier]
    Create a gesture.  The arguments are the name of the button to react
    upon and a modifier object to specify the shift-, control- and
    meta-keys.  When these arguments are omitted, they are read from their
    related class-variables.

    Specification through class_variable/4 is advised as this allows
    the user to set his/her own preferences and it will give all
    similar gestures consistent user-interface.

    Creating instances of this class is no common use.  Note however that a
    recogniser for a specific application may be created by creating an
    instance of class recogniser and attaching the necessary ->initiate,
    ->verify, ->drag and/or ->terminate methods to it using `object ->send_method`.

    @see gesture.modifier
    @see gesture.button

- gesture->initiate: event
    Invoked by ->event on a button-down event when all conditions are
    verified.  This method should perform necessary initialisation for the
    gesture.

    `gesture ->initiate` just succeeds.  Intended to be redefined by
    gesture's subclasses.

    @see gesture->event
    @see gesture->verify

- gesture->terminate: event
    Invoked by ->event when the matching button-up event that terminates the
    gesture action occurs.

    `gesture ->terminate` just succeeds.  Intended to be redefined by
    gesture's subclasses.

    @see gesture->event

- gesture->verify: event
    Invoked by ->event on the button-down event after the button, modifier
    and condition have been verified.  This method may do additional
    verification.  It is supposed to succeed or fail without side-effects.
    Side-effects necessary during initiation of the gesture should be
    defined at ->initiate.

    `gesture ->verify` just succeeds.  Intended to be redefined by
    gesture's subclasses.

    @see gesture->event
    @see gesture->initiate

