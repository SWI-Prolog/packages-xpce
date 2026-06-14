# class slider {#class-slider}

A slider is a graphical representation of a numeric value in a
predefined range.  The value can be changed by dragging the slider.
Sliders are commonly used to view/edit numeric values that
should not be exact (it is difficult to drag to an exact value).
Examples are the speed of something or a percentage(done).

A slider selects or visualises an numeric value in the range
(<-low, <-high).  Its current value is (as with all dialog_item
objects) accessible through <->selection.  The exact value is
normally shown in textual form as well.  This may be controlled
by <->show_value.  ->show_label determines whether or not the
label visible.

If either <-low or <-high is a real object, the slider will
assume it operates on floating point numbers.  If both <-low and
<-high is integer, the slider will assume integer values.  So,
to specify a slider from 0 to 10 in floating point, use:

	?- new(S, slider(value, 0,0, 10.0, 5.0)).

When a slider is operated by the user (by dragging the value), <-message
may be send either on termination or at each drag-event.  This is
controlled by <->drag.  When message is executed, the arguments are:

	| @receiver | The slider              |
	| @arg1     | The current <-selection |
	| @arg2     | The event object.       |

**Bugs**: Sliders should be able to handle ranges of floating-point numbers too.

@see class scroll_bar
@see class dialog_item
@see class text_item


## Instance variables {#class-slider-instvars}

- slider-default: int|real|function
    The default selection or function to get it.  Used by ->restore.

- slider<->drag: bool
    When @on, <-message will be activated on each drag-event received by the
    slider.  Otherwise, <-message will only be sent when the mouse-button is
    released.

    **Defaults**: @off (only send at mouse-up).

- slider<-format: [name]
    Format specification used to represent the textual values.  The
    specification is as for the C printf() call (see also
    `string->format`).  The default format for floating point
    values is %f and for integers %d.  The following defines a
    slider in 0.01 steps from 0.0 .. 1.0:

    	?- new(S, slider(value, 0.0, 1.0, 0.5)),
    	   send(S, format, '%.2f').

- slider<-high: int|real
    Together with <-low this variable describes the (including) range.

    @see slider-selection
    @see slider-low

- slider<-label_font: font*
    Font used to print the <-label if <-show_label equals @on.

    @see slider-show_label

- slider<-low: int|real
    Lowest value for <->selection.  See ->high.

    @see slider-selection
    @see slider-high

- slider<-selection: int|real
    Current value.  An integer for which <-low <= <-selection <= <-high.

    @see slider-high
    @see slider-low

- slider<-show_label: bool
    When @on, the <-label of the slider is printed at the left-side in
    <-label_font.

    **Defaults**: @on

    @see slider-show_value
    @see slider-label_font

- slider<-show_value: bool
    When @on, the current value is printed in textual format too.  It will
    be printed in <-value_font.

    **Defaults**: @on

    @see slider-value_font
    @see slider-show_label

- slider<-value_font: font
    @see slider-show_value

- slider-width: int
    Length of the bar-part of the slider in pixels.

    **Defaults**: 200 pixels.


## Send methods {#class-slider-send}

- slider->compute
    Computes the layout and sets <-area and <-reference using the various
    attributes.

- slider->default: value=int|real|function
    Sets the -default value and the ->selection.

- slider->event: event
    A slider is operated by the left pointer-button.  It will execute it's
    <-message when the user releases the button (<-drag: @off) or
    anytime the user moves the pointer with the button held down
    (<-drag: @on).

    First tries `dialog_item <-event`.

- slider->initialise: name, low=int|real, high=int|real, value=int|real|function, message=[code]*
    Create a slider object from its <-label, <-low, <-high and
    ->default.

- slider->modified: bool
    *Inherits description from*: slider<-modified


## Get methods {#class-slider-get}

- slider<-default: -> int|real
    If -default holds a function, execute it and return the result.
    Otherwise return the plain value of -default.  Used by ->restore.

- slider<-modified: -> bool
    A slider is considered modified if the currently displayed value
    (<-selection) is not equal to the <-displayed_value.

    ->modified: @off sets <-displayed_value to <-selection.

