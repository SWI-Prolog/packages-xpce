# class edit_text_gesture {#class-edit_text_gesture}

Gesture that drives in-place editing of a `text` graphical: a press
positions the caret (and may activate the text), a drag extends a
selection from the press point, and a release optionally activates
the editor.  Keystrokes are dispatched to the underlying text via
the standard text-editing recognisers.

@see class gesture
@see class text
@see class editor


## Instance variables {#class-edit_text_gesture-instvars}

- edit_text_gesture<->selection_origin: int
    Character index in the text at which the current selection was
    started (the press position).

- edit_text_gesture<->max_drag_distance: int*
    If the mouse moves less than this many pixels between press and
    release, the gesture treats the event as a click rather than a
    drag (the existing selection is preserved).  `@nil` disables the
    threshold.

- edit_text_gesture<->activate: bool
    If `@on`, `->terminate` activates the text on a click that did
    not start a drag.


## Send methods {#class-edit_text_gesture-send}

- edit_text_gesture->initialise: button=[button_name], modifier=[modifier]
    Create from button and modifier, default `left` with no
    modifier.

- edit_text_gesture->event: event
    Top-level event dispatch: handles typing and selection
    management.

- edit_text_gesture->initiate: event
    On a button press, clear the existing selection and move the
    caret to the press position.

- edit_text_gesture->drag: event
    On a drag, extend the selection from `<-selection_origin` to
    the current event position.

- edit_text_gesture->terminate: event
    On a release, activate the text if `<-activate` is `@on` and
    the gesture stayed within `<-max_drag_distance` of the press.


## Class variables {#class-edit_text_gesture-classvars}

- button: left
    Button on which this gesture is active.

- cursor: @default
    Cursor used while the gesture is active.

- max_drag_distance: 5
    Default click/drag threshold in pixels.
