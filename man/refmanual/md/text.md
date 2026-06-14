# class text {#class-text}

Class text is used to display (short) text fragments on a graphical
device object.

The text may be printed in any font.  It may span multiple lines
(separated by newline-characters (ASCII value 10)), which may be aligned
on their left-side, center or right-side.  The current
implementation silently truncates text objects that span more
than 200 physical lines.

Text objects can be boxed by setting their ->pen.

Text objects provide limited editing capabilities which are compatible
to the editing capabilities provided by class editor (EMACS oriented).
Text objects are edited using the method ->typed.

See also class text_item (text entry field), class view and class
editor.

@see class key_binding
@see text<-key_binding
@see text->typed
@see text.key_binding
@see class editor
@see class string
@see class text_item


## Class variables {#class-text-classvars}

- text.font: font = normal
    @see text-font

- text.key_binding: string =
    @see text->typed
    @see class text


## Instance variables {#class-text-instvars}

- text<-background: [colour|pixmap]*
    Defines the background of the text.  Allowed values are: @nil
    (transparent), @default (cleared to current background) or a
    colour object or a pixmap object.  See also ->transparent.

- text<-caret: int
    Location of the caret (= insertion point).  Zero indicates before the
    first character.  Setting the ->caret to @default places the caret at
    the end of the text.

    @see text-show_caret

- text<-font: font
    Font used to draw the text.  There are no provisions for multiple fonts
    in text object.  Note that class editor is capable of handing multiple
    fonts.

    **Defaults**: Resource defined.

    @see text.font
    @see class font

- text<-format: {left,center,right}
    The `format` describes how the lines of a text containing multiple lines
    are aligned and how the area of the text changes if the string or font
    is changed:

    - left
    	The left-sides of the lines are aligned.  It the text
    	changes, the top-left corner is maintained.

    - center
    	The lines are centered below each other, If the text
    	changes, the center is maintained.

    - right
    	The right-sides of the lines are aligned.  It the text
    	changes, the top-right corner is maintained.

    @see menu-format

- text<-margin: int
    Together with <-wrap, <-margin controls how long strings are represented
    by class text.   These slots are manipulated by ->margin.

- text-position: point
    Remember the reference point for resizing the text if it's contents
    changes.  Recomputing this point each time from the area would cause the
    text to `walk` due to rounding errors.

- text<-show_caret: bool|{passive}
    If @on, the caret (insertion-point) is displayed.  See also
    `window ->keyboard_focus` and ->caret.

    **Defaults**: @off

    @see text-caret

- text<-string: char_array
    The currently displayed string.

- text<-wrap: {extend,wrap,wrap_fixed_width,clip}
    Describes whether long strings are displayed as-is, clipped or wrapped
    to multiple lines.  See ->margin.

    @see text->margin

- text-x_caret: int
    X-position of the caret in pixels.  Used to avoid recomputing when the
    text has to be redisplayed.

    @see text-y_caret

- text-x_offset: int
    When <->length is specified (i.e.  non-zero), this value indicates
    how many pixels at the left-side are not visible.

    @see text-length

- text-y_caret: int
    Y-position of the caret in pixels.  Used to avoid recomputing when the
    text has to be redisplayed.

    @see text-x_caret


## Send methods {#class-text-send}

- text->backward_kill_word: times=[int]
    Deletes words backward from caret (\e DEL)

- text->caret: [int]
    @see text->typed
    @see text<-pointed

- text->catch_all: selector=name, argument=unchecked ...
    Delegate to <-string.  In addition, if <-string does not accept
    the method, but class string does, the <-string is converted
    to a string object holding the same characters.  See also
    ->has_send_method.

- text->compute
    Recomputes <-area from the <-string, <-font and <-margin.

- text->event: event
    The default event-handling for a text manipulates the visibility
    of the caret on focus events and invokes ->typed on itself if
    the event if a keyboard event and the <-show_caret yields
    @on.  See also ->keyboard_focus.

- text->format_left
    Set left alignment.

- text->format_right
    Set right alignment.

- text->format_center
    Format the text centered, left or right.  These methods are defined
    without arguments for invoking them easily from the keyboard.

- text->geometry: x=[int], y=[int], width=[int], height=[int]
    Just moves the text object as the size is determined by the <-string,
    <-font and <-margin.

- text->has_send_method: name
    Test whether the text object implements a method.  Text
    accepts all methods of itself, of <-string and of class
    string.  See also ->catch_all.

- text->initialise: string=[char_array], format=[{left,center,right}], font=[font]
    Create a text object from the specified string, format and font.  The
    <-string defaults to '', the <-format to `left` and the <-font to the
    text.font

- text->length: int
    Sets the <-margin in the <-fonts `ex` units (see `font <-ex`).  New code
    should use the more advanced ->margin method.

- text->margin: int*, [{wrap,wrap_fixed_width,clip}]
    Define how the text object handles long string.  Text objects may handle
    long strings in three different ways as described by <-wrap:

    - extend (->margin: @nil).
    	The graphical is as big as needed to display the string

    - clip (->margin: width, clip)
    	The graphical has fixed width.  If the string is too long it
    	will be clipped.

    - wrap-1 (->margin: width, wrap)
    	The graphical has a <-margin defined.  If the string is too long
    	to fit within the margin it will be wrapped on multiple physical
    	lines.  Wrapping only happens at word boundaries.

    - wrap-2 (->margin: width, wrap_fixed_width)
    	Similar to `wrap-1`, but the <-width of the graphical
    	equals the margin rather than the width occupied by the
    	displayed characters.

    @see text-wrap

- text->paste: which=[{primary,clipboard}]
    Paste value of the numbered X-cut-buffer.  See `display <-cut_buffer`.

- text->resize: factor_x=real, factor_y=[real], origin=[point]
    @see graphical->resize

- text->selection: from=[int]*, to=[int]
    Make [from, to) the selection.  If `from` is @nil to must be
    omitted and the selection is cleared.  Otherwise default
    values do not change the corresponding end of the selection.

    See also <-selected_text, class edit_text_gesture and the
    Prolog library(pce_editable_text).

- text->show_caret: bool|{passive}
    @see text->typed
    @see text<-pointed

- text->transparent: bool
    Compatibilty method defining <-background.  The value @on is
    equivalent to ->background: @nil and @off to ->background:
    @default.

- text->typed: event|event_id
    Handle keyboard event.  Class `text` does not provide default
    event-handling, but editable text objects are easily created by
    forwarding keyboard-events to this method:

    	:- pce_global(@edit_text_recogniser,
    				  new(handler(keyboard,
    				 			  message(@receiver,
    							  	      typed,
    								      @arg1)))).

    	text_make_editable(T) :-
    		send(T, recogniser, @edit_text_recogniser).

    This method uses the key_binding object called `text` from @key_bindings
    to map the typed keys on selectors and then invokes methods on class
    text.  With the above, if the user types control-U to the text the
    following happens:

    1. Event arrives at text and is given to ->typed using
     	@edit_text_recogniser.

    2. The `event <-id` is processed by the `text` key_binding
    	object and mapped on the selector `clear`.  ->clear does
    	not require arguments and the key_binding object just
    	invokes `text->clear`.

    3. ->clear erases all characters from the text.

    **Diagnostics**: Fails if the keyboard event could not be handled.

    @see text->show_caret
    @see text->caret
    @see text<-pointed
    @see text.key_binding
    @see text<-key_binding
    @see class text_item
    @see class text

- text->underline: bool|colour
    The text is underlines if @on using the same colour as the text
    or using an explicit colour if the value is a colour object.


## Get methods {#class-text-get}

- text<-pointed: at=point, round=[bool] -> index=int
    Returns character index from a pixel-position relative to the top-left
    corner (origin) of the text.  This method may be used to set the caret
    using the mouse:

    	?- new(T, text('Hello World')),
    	   send(T, show_caret, @on),
    	   send(T, recogniser,
    	        click_gesture(left, '', single,
    						  message(T, caret,
    								  ?(T, pointed,
    								    @event?position)))).

    @see text->typed
    @see text->show_caret
    @see text->caret

- text<-selected_text: -> string
    New string object with contents of <-selection

- text<-selection: -> point
    New point object with start and end of selection.  See also
    ->selection.

