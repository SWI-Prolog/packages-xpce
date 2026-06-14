# class button {#class-button}

A button appears on the screen as a rectangle with a textual label.
When the user presses the button, it will execute its associated message
with the following parameters:

	@receiver		The button itself

If <-message is @default, the button will send its <-name to its
<-device.  Thus, a button named `apply` will invoke ->apply on the
dialog box it is displayed on.

A button may be assigned a key-sequence to activate it.  See
<-accelerator.  If the <-accelerator is "RET" the button is the
`default` button of its device and will be highlighted.  See also
`dialog ->default_button`).

A button may be assigned a popup menu using ->popup.  Existence of a
popup menu is indicated using a small triangle.

**Bugs**:

It is not possible to use an arbitrary bitmap as a button.  Class label
provides a (conceptually inferior) way to create `active` images.

@see class click_gesture


## Instance variables {#class-button-instvars}

- button<-accelerator: [name]*
    Activate when ->key: name is received.  See `dialog ->typed` for an
    explanation on handling accelerators in dialog windows.

- button<-default_button: [bool]
    Button is default button for its <-device.  This implies two
    things.  First of all, if an item in the dialog is modified, the
    item will invoke `dialog ->modified_item`.  If the dialog
    finds a `dialog <-default_button`, it will make the
    default button ->active.  If the RETURN key is hit in the
    dialog window, the default button will be ->execute'd.

- button<-popup_image: image*
    Indication that button has a popup menu.  If present this image
    is painted right-of the label of the button.

- button<-show_focus_border: bool
    By default, the gtk and motif styles show a wide sunken
    border around the default button or the button holding the
    keyboard focus.  By setting this variable to @off, this
    border is never painted.  This is intended to deal with
    tightly placed buttons, buttons at the very edge of a window,
    etc.

    The method ->label switches this flag automatically to @off
    if the label is set to an image.


## Send methods {#class-button-send}

- button->compute
    Computes the <-width and <-height using the <-name and <-font.  If the
    required size turns out to be smaller than button.size this size is used instead.

- button->default_button: [bool]
    *Inherits description from*: button-default_button, button<-default_button

- button->event: event
    First calls `dialog_item ->event`.  If this fails it passes the event to
    @_button_gesture (a click_gesture object).

- button->execute
    If <-message is not @nil, ->execute acts as a wrapper around
    ->forward, dealing with the visual feedback.  The visual
    feedback consist of switching ->status to `execute` and setting
    the busy-cursor (using `display ->busy_cursor`).

- button->forward
    Perform associated action.  The action depends in <-message:

    - @nil
    	No action is performed

    - @default
    	<-name is sent to <-device

    - a code object
    	The code object is executed, while @receiver is bound to
    	the button object.

- button->initialise: name=name, message=[code]*, label=[name]
    Create a button with the specified <-name.  The displayed <-label is
    computed from this name using `char_array <-label_name` from the
    <-name.

    When the button is depressed (using the left-button) it will ->execute
    itself, activating the given code object.

- button->key: key=name
    Normally invoked from `dialog ->typed`.  _Key_ is the symbolic name of
    the key(-sequence) typed.   If this `key` is equal to <-accelerator,
    ->execute the button.

- button->popup: popup*
    Assign <-popup and update (add/remove) the indication on the button for
    the existence of a popup menu.

- button->selection: char_array|image*
    *Inherits description from*: button<-selection

- button->status: {inactive,active,preview,execute}
    Assign <-status and redraw:

    	| active and inactive | _Normal_ appearance    |
    	| preview             | Inverted inside        |
    	| execute             | Grey inside background |


## Get methods {#class-button-get}

- button<-popup: create=[bool] -> popup*
    Get the popup object associated with the button.  If the -popup
    slot has value @nil and `create` is @on,  a popup is associated
    with the button.  This popup has one item with the following
    attributes:

    	`menu_item <-value`		<-name
    	`menu_item <-label`		<-label
    	`menu_item <-message`	message(@arg1, execute)

    The message will ->execute the button when the first item
    is selected.

- button<-selection: -> name
    Equivalent to <->label.  For compatibility with older versions as well
    as for compatibility with other dialog_item's.

