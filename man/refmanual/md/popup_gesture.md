# class popup_gesture {#class-popup_gesture}

A `popup_gesture` is used to show a popup object.  It `event ->posts`
all incoming events to the popup-menu and requests the popup to execute
the selected command after the user releases the button that caused the
gesture to activate the popup.

Many classes define methods for associating popup objects using the
method `graphical ->popup` or a redefinition thereof.  Direct use of
instances of this class is often not necessary.

@see graphical->popup
@see class popup


## Instance variables {#class-popup_gesture-instvars}

- popup_gesture<->context: any
    The argument for which the popup is ran.  Normally this is the receiving
    graphical object.   However, it is possible to rebind the context in the
    ->initiate method.  For example, class list_browser sets the <-context
    to the dict_item object under the pointer.

    See also ->terminate.

- popup_gesture<->max_drag_distance: int*
    If @nil, the popup shows immediately on the down event and may
    be selected by dragging to an item.  If an integer the gesture
    behaves like a click_gesture object.  If the user drags more
    than the specified distance the gesture is ->cancel'ed
    otherwise the menu is displayed on the `up` event.

- popup_gesture<->popup: popup|function*
    Popup displayed by the gesture.  If is a function object, it is
    evaluated by ->verify using the following arguments:

    	| @arg1 | `event <-master` (normally receiving graphical) |
    	| @arg2 | The event (normally @event)                     |

    The slot -current is filled with the result.


## Send methods {#class-popup_gesture-send}

- popup_gesture->drag: event
    Drag just passes incoming drag-events to the displayed popup menu.

- popup_gesture->event: event
    First runs `gesture ->event`.  When not successful and the event is a
    keyboard event, `popup ->key` will be invoked with the key description.
    This facility makes keyboard accelerators associated with the popup
    active.

- popup_gesture->initialise: popup=[popup|function], button=[button_name], modifier=[modifier]
    Create from popup object, button name and modifier.  By default is a
    popup associated to the right button.   Note that the popup argument
    may be a function.  See <-popup.

- popup_gesture->initiate: event
    Initiate performs the following actions:

    - `popup ->update: Context`
    	Allow the popup to change it's menu_items before it is shown.

    - `popup ->open: Receiver, Position`
    	Actually display the popup.  The receiver passed is the receiver
    	of the event, the position is the position of the event,
    	relative to the receiver.

    - `event ->post: Popup`
    	Post the event to the popup

    @see popup->open
    @see popup->update

- popup_gesture->terminate: event
    The terminate method performs the following:

    - Set the <-context and -current variables to @nil
    - `event ->post` to the -current popup menu
    - `popup ->execute` the menu, providing the context

    @see popup->execute

- popup_gesture->verify: event
    Verify tries to find the popup-menu to be displayed and sets the
    -context attribute of the popup_gesture to the event-receiver if it is
    not set to @nil.

    If the <->popup attribute of the gesture is set, this popup menu is
    used.  If it is a function object, the function is evaluated.  See
    <-popup.

    Otherwise, the `event <-master` is requested a popup with
    <-popup.

    After finding a popup object, this object is sent `popup ->update` to
    update its menu_items.  If, after `popup <-active = @off` or `popup
    <-members' is empty, ->verify fails.

