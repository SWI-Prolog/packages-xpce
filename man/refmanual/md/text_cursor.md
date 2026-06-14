# class text_cursor {#class-text_cursor}

A text_cursor is used to visualise the `editor <-caret` in an editor
object.  Class text_cursor is closely linked to class editor and not
meant to be used outside the context of editors.

The only interesting behaviour to the application programmer is:

	| ->style | Change the `look` of the caret |
	| ->image | Create custom caret.           |

**Bugs**:

The behaviour of this class is not complete.  It performs satisfactory
in the context of class editor, but is not very generic.


## Instance variables {#class-text_cursor-instvars}

- text_cursor<-active: bool
    Indicate whether or not typing will affect this caret.  The visual
    feedback depends on the <-style.

- text_cursor<-hot_spot: point*
    When <->style is `image`; this point describes how the image object is
    positioned relative to the character.

- text_cursor<-image: image*
    When <->style is `image`; this is the image object.

- text_cursor<-style: {arrow,image,block,open_look}
    Style of the text_cursor.  Values are:

    - arrow
    	A small triangle with a vertical line.  Normally used
     	with proportional fonts.

    - open_look
    	Triangle when the cursor is ->active, grey diamond
    	otherwise.  Used for OpenLook compatibility.

    - block
    	Static inverting block,  Normally used with fixed-width
    	fonts on monochrome screens.

    - bitmap
    	Arbitrary pattern.

    The default style used by an editor is determined by the font
    associated with the editor.  See  `text_cursor ->font`.


## Send methods {#class-text_cursor-send}

- text_cursor->font: font
    Sets the ->style according to the font.  If the font has
    `font<-fixed_width: @on` font, it will invoke ->style using
    font.fixed_font_style.  Otherwise it will use font.proportional_font_style.

- text_cursor->image: image*
    @see text_cursor->style

- text_cursor->initialise: for=[font]
    Create a text_cursor object from the specified font object.
    Class text_cursor is currently only used by class editor.
    See also ->font and `editor <-text_cursor`.

- text_cursor->set: x=int, y=int, width=int, height=int, baseline=int
    Specify all dimension parameters of the text_cursor.  This method is
    used by class editor to position the caret.  The x, y, width and height
    parameters describe the character-box.  baseline is the depth of
    the baseline of the current font.

- text_cursor->style: {arrow,image,block,open_look}
    @see text_cursor->image

