# class click_gesture {#class-click_gesture}

A `click_gesture` can be used to make any graphical behave as a button.
It allows you the mouse-button that should be recognised; the state of
the modifier buttons and the `multiclick` status (single, double,
triple clicks).

The above forms the `condition` part.  The `action` part consists of
three messages.  When the mouse goes down, the <-preview_message is
sent.  Next, when the mouse goes up inside the area of the graphical,
the <-execute_message is activated, otherwise the <-cancel_message.
For all these messages, the following arguments are forwarded:

	| @receiver | The receiving graphical object |
	| @arg1     | The event                      |

Normally, the <-preview_message indicates that releasing the button here
will start some command.  The execute message will execute the command
and remove the feedback provided by the preview and the cancel message
just removes this feedback.

@see event<-multiclick
@see class button


## Instance variables {#class-click_gesture-instvars}

- click_gesture<->cancel_message: code*
    The cancel_message is executed when the user releases the button that
    initiated this click gesture outside the area of the graphical this
    gesture is associated with.  When executed, the following arguments are
    forwarded:

    	@receiver		Graphical on which the gesture was started
    	@arg1		Event

    When @nil, no cancel message is sent.

    **Defaults**: Default is @nil (no cancel message is sent)

- click_gesture<->execute_message: code*
    This message is sent when the user releases the button that initiated this
    gesture inside the area of the graphical.  The argument bindings are:

    	| @receiver | The graphical |
    	| @arg1     | The event     |

    When @nil, no message is sent.

    **Defaults**: @nil (no message)

- click_gesture<->max_drag_distance: int*
    This variable is used by ->drag to cancel the click_gesture
    after the user dragged too far.   @nil implies the gesture is
    never canceled by dragging.

- click_gesture<->multiclick: [{single,double,triple}]
    The multiclick attribute determines whether the gesture reacts on single, double
    or triple click.

    **Defaults**: single

- click_gesture<->preview_message: code*
    The preview message is sent when the gesture is initiated (i.e. on the down
    event).  The following arguments are passed:

    	| @receiver | The graphical |
    	| @arg1     | The event     |

    When @nil, no message is sent.

    **Defaults**: @nil (no message is sent).


## Send methods {#class-click_gesture-send}

- click_gesture->cancel: event
    Forward <-cancel_message (if any) and cancel the gesture using
    `gesture ->cancel`.  Called from ->drag if the user dragged too
    far.

- click_gesture->drag: event
    If <-max_drag_distance is not @nil, determine the distance
    between the original down event and the current event and
    ->cancel if this distance exceeds <-max_drag_distance
    pixels.

- click_gesture->initialise: button=[button_name], modifier=[modifier], multiple=[{single,double,triple}], message=[code]*, preview=[code]*, cancel=[code]*
    Initialises a click_gesture from the following arguments:

    	| button          | Mouse-button: {left,middle,right}         |
    	| modifier        | Modifier status                           |
    	| multiclick      | Multiclick status: {single,double,triple} |
    	| execute_message | Ran when click is finished (up)           |
    	| preview_message | Ran when click is started (down)          |
    	| cancel_message  | Ran on up outside area                    |

    **Defaults**:
    	button		Resource defined (left)
    	modifier		Resource defined (none)
    	multiclick		Resource defined (single)
    	execute_message	@nil
    	preview_message	@nil
    	cancel_message	@nil

- click_gesture->initiate: event
    When <-preview_message is not @nil, forward this message.

- click_gesture->terminate: event
    Executes <-execute_message iff the pointer is not moved outside the
    area of the graphical for which this gesture was started *or* the
    pointer has been moved less than 5 pixels.

    Otherwise the <-cancel_message is executed when it is not @nil

