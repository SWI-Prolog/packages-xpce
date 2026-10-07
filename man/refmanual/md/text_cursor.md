# class text_cursor {#class-text_cursor}

A text_cursor is used to visualise the `editor <-caret` in an editor
object.  Class text_cursor is closely linked to class editor and not
meant to be used outside the context of editors.

The class variables of text_cursor also define the caret of class
text (e.g., a text_item) and class terminal_image.

The only interesting behaviour to the application programmer is:

	| ->style | Change the `look` of the caret |
	| ->image | Create custom caret.           |


## Class variables {#class-text_cursor-classvars}

- text_cursor.fixed_font_style: {bar,block,underline,xpce} = xpce
    How the caret is visualised if the font is fixed width.  See
    <-style for the values.

- text_cursor.proportional_font_style: {bar,block,underline,xpce} = xpce
    How the caret is visualised if the font is proportional.  See
    <-style for the values.

- text_cursor.blink: bool = @on
    If @on, the caret that has the keyboard focus blinks.  Moving the
    caret or typing makes it visible and restarts blinking.

- text_cursor.blink_interval: 1.. = 500
    Milliseconds the caret is shown and hidden while blinking.

- text_cursor.blink_timeout: 0.. = 10
    Stop blinking after this many seconds without moving the caret.
    The caret then remains visible.  If 0, the caret blinks forever.

- text_cursor.colour: colour = ui_cursor
    Colour of the caret if it is ->active.

- text_cursor.inactive_colour: colour = ui_cursor_inactive
    Colour of the caret if it is not ->active, i.e., the editor
    does not have the keyboard focus.

- text_cursor.height: int = 11
    Size of the caret if <-style is `xpce`.


## Instance variables {#class-text_cursor-instvars}

- text_cursor<-active: bool
    Indicate whether or not typing will affect this caret.  The visual
    feedback depends on the <-style.

- text_cursor<-hot_spot: point*
    When <->style is `image`; this point describes how the image object is
    positioned relative to the character.

- text_cursor<-image: image*
    When <->style is `image`; this is the image object.

- text_cursor<-style: {bar,block,underline,xpce,image}
    Style of the text_cursor.  Values are:

    - bar
    	A thin vertical bar before the character at the caret.

    - block
    	A translucent box over the character at the caret.  If
    	the caret is not ->active, the box is drawn as an outline.

    - underline
    	A line below the character at the caret.

    - xpce
    	The classic xpce caret: a small triangle below the
    	insertion point if the cursor is ->active, a diamond
    	otherwise.

    - image
    	An arbitrary image, set using ->image.

    An inactive caret uses text_cursor.inactive_colour.  The style
    is set by ->font from text_cursor.fixed_font_style or
    text_cursor.proportional_font_style.


## Send methods {#class-text_cursor-send}

- text_cursor->font: font
    Set the <-style from text_cursor.fixed_font_style or
    text_cursor.proportional_font_style, depending on
    `font <-fixed_width`, and the initial size of the caret.
    If the <-style is `image`, it is not changed.

- text_cursor->active: bool
    The caret is active if its editor has the keyboard focus.
    An active caret blinks; see text_cursor.blink.

- text_cursor->image: image*
    @see text_cursor->style

- text_cursor->initialise: for=[font]
    Create a text_cursor object from the specified font object.
    Class text_cursor is currently only used by class editor.
    See also ->font and `editor <-text_cursor`.

- text_cursor->set: x=int, y=int, width=int, height=int, baseline=int
    Specify all dimension parameters of the text_cursor.  This method is
    used by class editor to position the caret.  The x, y, width and height
    parameters describe the box of the character at the caret.  baseline
    is the depth of the baseline of the current font.

- text_cursor->style: {bar,block,underline,xpce,image}
    @see text_cursor->image
