# class label {#class-label}

A label is a dialog_item object used to inform the user.  It's selection
is either a string or an image.  This selection is the only
visual part of a label.  Labels are normally used to provide
feedback inside a dialog window or to describe a group of
dialog_items.

Labels play a role in the predefined (error) reporting system initiated
by `object ->report`.  If a label object named `reporter`
appears in a dialog object, the dialog's `dialog <-report_to`,
combined with `frame <-report_to` ensures that all status,
warning and error messages initiated by ->report will be
displayed by this label.  See ->report and `visual ->report`.

It is possible to attach a message to a label, which is than executed
when the user presses the label with the left button.  This use is not
advocated.


## Instance variables {#class-label-instvars}

- label<-font: font
    Font used to display the label.  Note that the font is only applicable
    if the <-selection's value is a char_array object (text).

- label<-length: 0..
    Width of the label's field in `ex` units (i.e.  width of the character
    `x` in the current <-font).

- label<-selection: char_array|image
    Currently displayed text or image object.  If the selection is a text
    (instance of class char_array), it is displayed in <-font.  The field
    width is described by <-length.  If the actual text does not fit in the
    field the label is automatically enlarged.


## Send methods {#class-label-send}

- label->catch_all: name, unchecked ...
    Messages not understood are delegated to the current <-selection.  If
    the selection is a name object and the method is known to class string,
    the selection will automatically be translated into a string object.
    This implies that a label that displays a name still understands all
    methods defined on class string.  Notably `string->append`, and
    `string->format` are commonly used methods.

- label->compute
    Computes the <-area of the label from the <-selection.  If the selection
    is text and the width of the text is smaller than <-length times
    <-font<-ex, the width is made equal to this value.  This will reserver
    sufficient space when the label is placed by the automatic `dialog
    ->layout' mechanism.

- label->event: event
    Make the button behave as a button object if <-message is defined.

    First invokes `dialog_item ->event`.  If this fails and <-active == @on
    and <-message is not @nil, forward  <-message on a left-click.
    Uses @_button_gesture.

- label->execute
    Execute <-message.  Arguments:

    	@receiver	the label

    This method is normally invoked from ->event through a click_gesture
    object.

- label->format: name, any ...
    Create a formatted string from the arguments and make it the
    <-selection.  Equivalent to:

    	send(Label, selection, string(Format, Argument ...)).

- label->initialise: name=[name], selection=[string|image], font=[font]
    Create a label from its name (used to reference it in its device; see
    `device <-member`) and its selection.  The default name is `reporter`,
    which makes the label the reporter of the dialog (see `dialog <-report_to`).
    The default selection is ''.  The default font is determined by
    label.font.

    The width a label object requires on the window is determined by
    ->length, whose default is determined by the resource
    label.length.

- label->report: kind={status,inform,progress,done,warning,error,fatal}, format=[char_array], argument=any ...
    ->format the message in the label. If the type of the information is
    either warning or error it will invoke `graphical ->flash` on @reportee.

    Note that class dialog redefines <-report_to (see `dialog <-report_to`).
    If a label is called `reporter` (see `dialog_item <-name`), the dialog
    will delegate ->report messages to this label.

    _Kind_ is progress is equivalent to `status`, but sends ->flush
    to update the display immediately after changing the
    <-selection.   _Kind_ is done appends the formatted text to the
    current <-selection of the label.  If `format` is @default,
    `done` is appended to the selection.

    @see dialog<-report_to

- label->status: {inactive,active,preview,execute}
    Status of the label with regard to event processing.  Not used.

- label->width: [0..]
    Width of the label in pixels.  This is used to achieve clean
    alignment.   If <-selection is an image object the image
    is scaled to this width and the height is scale proportionally.

