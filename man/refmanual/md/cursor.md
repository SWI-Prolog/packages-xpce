# class cursor {#class-cursor}

Class cursor provides access to the cursors of the system and allows
the user to define new cursors from an image.  The predefined cursors
are the standard (SDL system) cursors and the names that are kept for
compatibility with the X11 cursor font, which map to a standard cursor.
@cursor_names lists them; `cursor <-kind` tells which is which.

Cursors are assigned at the level of graphicals.  Given some position of
the pointer, the displayed cursor is determined to be first graphical
that will receive pointer-events from this position and that has a
`graphical <->cursor` attribute not equal to @nil or @default.
Normally, this is a graphical that overlaps with the pointer position
and is exposed (i.e.  above the other graphicals).  When the event-focus
is set for some device, the cursor is set to the cursor attribute of the
graphical in focus, provided it's value is a cursor (and not @default or
@nil).

Like fonts and colours, cursors are reusable objects and are
automatically shared.  If the <-name of the cursor is @nil, the
cursor will not be shared and may be ->free'ed just like any other
object.

The type conversion conventions will automatically transform the name
of a predefined cursor to a cursor object.

@see graphical->focus_cursor
@see window->focus_cursor
@see graphical-cursor
@see @cursor_names
@see @cursors


## Instance variables {#class-cursor-instvars}

- cursor<-hot_spot: point*
    If the cursor is user-defined, this describes the offset from the
    top-left of the image to the location of the pointer-device.

- cursor<-image: image*
    If the cursor is user-defined, this image of `image <-kind: image`
    defines the cursor.

- cursor<-kind: {standard,x11,image}
    Where the cursor comes from: `standard` for a cursor of the
    system, `x11` for a name kept for compatibility with the X11 cursor
    font (it maps to a standard cursor) and `image` for a cursor
    defined from an image.

- cursor<-summary: char_array*
    Description of the cursor.  Set for the standard cursors.  An old
    X11 name has the description of the standard cursor it maps to.
    It may be set for a cursor defined from an image.

- cursor<-name: name*
    Name of the cursor.  All cursors (predefined or user-defined) are stored
    in the table @cursors.  Note that the names of the predefined cursors
    are in @cursor_names.

    When @nil, the name does not appear in @cursors.  The cursor is not
    reusable and may be destroyed using ->free.


## Send methods {#class-cursor-send}

- cursor->initialise: name=name*, image=[image], hot_spot=[point]
    This method takes two forms.  If only a name is specified, this should
    be a name of a cursor that is predefined in @cursor_names.  This
    provides access to the SDL predefined cursors.  See the demo
    program _Cursors_ for the available predefined cursors.  A
    typical usage is:

    	send(DebugIcon, cursor, not_allowed).

    In its second form it allows the user the define a cursor.  Such a
    cursor consists of:

    	| <-name       | Name for reference (see <-lookup).    |
    	| <-image      | Image object describing the image.    |
    	| <-hot_spot   | Position of the pointer in the image. |
    	| <-foreground | Colour for the _1_-s in <-image       |
    	| <-background | Colour for the _0_-s in <-image       |

    <-image *must* be specified.  <-hot_spot defaults to `image
    <-hot_spot', or otherwise to point(0,0).  Both colours default
    to the fore- and background colours of the display on which the
    cursor is opened and often need not be specified.

    If the cursor has a <-name, it is locked using ->protect.
    Otherwise the livetime of the cursor object follows the normal rules.

